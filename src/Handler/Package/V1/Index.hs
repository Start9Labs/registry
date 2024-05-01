{-# LANGUAGE DataKinds #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Handler.Package.V1.Index where

import Conduit (concatMapC, dropC, mapC, mapMC, runConduit, sinkList, takeC, (.|))
import Control.Monad.Reader.Has (Functor (fmap), Has, Monad ((>>=)), MonadReader, ReaderT (runReaderT), ask, lift)
import Data.Aeson (FromJSON (..), decode, eitherDecodeStrict, withObject, (.:))
import Data.Attoparsec.Text qualified as Atto
import Data.ByteString.Lazy qualified as LBS
import Data.Conduit.List qualified as CL
import Data.HashMap.Internal.Strict (HashMap)
import Data.HashMap.Strict qualified as HM
import Data.List (lookup)
import Data.List.NonEmpty qualified as NE
import Data.Text qualified as T
import qualified Data.MultiMap as MM
import Database.Persist.Sql (SqlBackend)
import Database.Queries (
    collateVersions,
    getCategoriesFor,
    getPkgDataSource,
    getPkgDependencyData,
    serviceQuerySource, getLatestVersionRecord,
 )
import Foundation (Handler, Route (InstructionsR, LicenseR), RegistryCtx (appSettings))
import Handler.Package.Api (DependencyRes (..), PackageListRes (..), PackageRes (..))
import Handler.Types.Api (ApiVersion (..))
import Handler.Util (basicRender, parseQueryParam, filterDeprecatedVersions, filterDevices, getPkgArch)
import Lib.PkgRepository (PkgRepo, getIcon, getManifest)
import Lib.Types.Core (PkgId, PkgId(PkgId))
import Lib.Types.Emver (Version, Version(Version), VersionRange (..), parseRange, satisfies)
import Model (Category (..), Key (..), VersionRecord (..), PkgRecord (..))
import Protolude.Unsafe (unsafeFromJust)
import Settings (AppSettings (communityVersion))
import Startlude (
    Applicative ((*>)),
    Bifunctor (..),
    Bool (..),
    ByteString,
    ConvertText (toS),
    Down (..),
    Eq (..),
    Int,
    Maybe (..),
    MonadIO,
    NonEmpty,
    Num ((*), (-)),
    Show,
    Text,
    const,
    encodeUtf8,
    filter,
    flip,
    for,
    fromMaybe,
    headMay,
    id,
    liftA2,
    mappend,
    nonEmpty,
    note,
    pure,
    readMaybe,
    snd,
    sortOn,
    zipWithM,
    ($),
    (&&&),
    (.),
    (.*),
    (<$>),
    (<&>),
    (=<<)
 )
import UnliftIO (Concurrently (..), mapConcurrently)
import Yesod (
    ContentType,
    MonadLogger,
    MonadResource,
    YesodPersist (runDB),
    lookupGetParam,
 )
import Data.Tuple (fst)
import Database.Persist.Postgresql (entityVal, entityKey)
import Yesod.Core (getsYesod)
import Yesod (YesodRequest(reqGetParams))
import Yesod (getRequest)
import Data.List (last)
import Data.Text (isPrefixOf)
import Control.Monad.Logger (logWarn)
import Data.String.Interpolate.IsString (
    i,
 )
data PackageReq = PackageReq
    { packageReqId :: !PkgId
    , packageReqVersion :: !VersionRange
    }
    deriving (Show)
instance FromJSON PackageReq where
    parseJSON = withObject "package version" $ \o -> do
        packageReqId <- o .: "id"
        packageReqVersion <- o .: "version"
        pure PackageReq{..}


data PackageMetadata = PackageMetadata
    { packageMetadataPkgId :: !PkgId
    , packageMetadataPkgVersionRecords :: !(NonEmpty VersionRecord)
    , packageMetadataPkgVersion :: !Version
    , packageMetadataPkgCategories :: ![Category]
    }
    deriving (Eq, Show)


getPackageIndexR :: Handler PackageListRes
getPackageIndexR = do
    osPredicate <-
        getOsVersionCompat <&> \case
            Nothing -> const True
            Just v -> flip satisfies v
    pkgArch <- getPkgArch
    ram <- getRamQuery
    hardwareDevices <- getHardwareDevicesQuery
    communityVersion <- getsYesod $ communityVersion . appSettings
    pkgIds <- getPkgIdsQuery
    category <- getCategoryQuery
    page <- fromMaybe 1 <$> getPageQuery
    limit' <- fromMaybe 20 <$> getLimitQuery
    query <- T.strip . fromMaybe "" <$> lookupGetParam "query"
    let (source, packageRanges) = case pkgIds of
            Nothing -> (serviceQuerySource category query pkgArch ram, const Any)
            Just packages ->
                let s = getPkgDataSource (packageReqId <$> packages) pkgArch ram
                    r = fromMaybe None . (flip lookup $ (packageReqId &&& packageReqVersion) <$> packages)
                in (s, r)
    filteredPackages <-
        runDB $
            runConduit $
                source
                    -- group conduit pipeline by pkg id
                    .| collateVersions
                    -- filter out versions of apps that are incompatible with the OS predicate
                    .| mapC (second (filter (osPredicate . versionRecordOsVersion . fst)))
                    -- filter hardware device compatability                        
                    .| mapMC (\(b,c) -> do 
                        l <- filterDevices hardwareDevices c
                        pure (b, l)
                        )
                    -- filter out deprecated service versions after community registry release
                    .| mapC (second (filterDeprecatedVersions communityVersion osPredicate))
                    -- prune empty version sets
                    .| concatMapC (\(pkgId, vs) -> (pkgId,) <$> nonEmpty vs)
                    -- grab the latest matching version if it exists
                    .| concatMapC (\(a, b) -> (a,b,) <$> (selectLatestVersionFromSpec packageRanges b))
                    -- construct
                    .| mapMC (\(a, b, c) -> PackageMetadata a b (versionRecordNumber c) <$> getCategoriesFor a)
                    -- pages start at 1 for some reason. TODO: make pages start at 0
                    .| (dropC (limit' * (page - 1)) *> takeC limit')
                    .| sinkList

    -- NOTE: if a package's dependencies do not meet the system requirements, it is currently omitted from the list
    pkgsWithDependencies <- runDB $ mapConcurrently getPackageDependencies filteredPackages
    PackageListRes <$> runConcurrently (zipWithM (Concurrently .* constructPackageListApiRes) filteredPackages pkgsWithDependencies)

getPkgIdsQuery :: Handler (Maybe [PackageReq])
getPkgIdsQuery = parseQueryParam "ids" (first toS . eitherDecodeStrict . encodeUtf8)


getCategoryQuery :: Handler (Maybe Text)
getCategoryQuery = parseQueryParam "category" ((flip $ note . mappend "Invalid 'category': ") =<< (readMaybe . T.toUpper))


getPageQuery :: Handler (Maybe Int)
getPageQuery = parseQueryParam "page" ((flip $ note . mappend "Invalid 'page': ") =<< readMaybe)


getLimitQuery :: Handler (Maybe Int)
getLimitQuery = parseQueryParam "per-page" ((flip $ note . mappend "Invalid 'per-page': ") =<< readMaybe)


getOsVersionCompatQueryLegacy :: Handler (Maybe VersionRange)
getOsVersionCompatQueryLegacy = parseQueryParam "eos-version-compat" (first toS . Atto.parseOnly parseRange)

getOsVersionCompatQuery :: Handler (Maybe VersionRange)
getOsVersionCompatQuery = parseQueryParam "os.compat" (first toS . Atto.parseOnly parseRange)

getOsVersionCompat :: Handler (Maybe VersionRange)
getOsVersionCompat = do
    osVersion <- getOsVersionCompatQuery >>= \case
        Just a -> pure $ Just a
        Nothing -> getOsVersionCompatQueryLegacy
    pure osVersion

getHardwareDevicesQuery :: Handler (MM.MultiMap Text Text)
getHardwareDevicesQuery = do
    allParams <- reqGetParams <$> getRequest
    -- [("hardware.device.processor","intel"),("hardware.device.display","led")]
    let hardwareDeviceParams = filter (\(key, _) -> "hardware.device" `isPrefixOf` key) allParams
    -- [("processor","intel"),("display","led")]
    pure $ MM.fromList $ first (last . T.splitOn ".") <$> hardwareDeviceParams

getRamQuery :: Handler (Maybe Int)
getRamQuery = parseQueryParam "hardware.ram" ((flip $ note . mappend "Invalid 'ram': ") =<< readMaybe)

getPackageDependencies ::
    (MonadIO m, MonadLogger m, MonadResource m, Has PkgRepo r, MonadReader r m) =>
    PackageMetadata ->
    ReaderT SqlBackend m (HashMap PkgId DependencyRes)
getPackageDependencies PackageMetadata{packageMetadataPkgId = pkg, packageMetadataPkgVersion = pkgVersion} =
    do
        depPkgRecordEntities <- getPkgDependencyData pkg pkgVersion
        fmap HM.fromList $
            for depPkgRecordEntities $ \(pr) -> do
                let depId = unPkgRecordKey $ entityKey pr
                let depPkgRecord = entityVal pr
                mVersionRecord <- getLatestVersionRecord $ entityKey pr
                case mVersionRecord of
                    Just VersionRecord{..} -> do
                        icon <- lift $ loadIcon depId versionRecordNumber
                        pure $ (depId, DependencyRes versionRecordTitle icon $ pkgRecordHidden depPkgRecord)
                    Nothing -> do
                        $logWarn [i|No latest version record found for #{depId} while getting dependency metadata for #{pkg}@#{pkgVersion}. Using fallback package.|]
                        icon <- lift $ loadIcon (PkgId "fallback") $ Version(1,0,0,0)
                        pure $ (depId, DependencyRes "Unknown" icon $ pkgRecordHidden depPkgRecord)

constructPackageListApiRes ::
    (MonadResource m, MonadReader r m, Has AppSettings r, Has PkgRepo r) =>
    PackageMetadata ->
    HashMap PkgId DependencyRes ->
    m PackageRes
constructPackageListApiRes PackageMetadata{..} dependencies = do
    settings <- ask @_ @_ @AppSettings
    let pkgId = packageMetadataPkgId
    let pkgCategories = packageMetadataPkgCategories
    let pkgVersions = packageMetadataPkgVersionRecords
    let pkgVersion = packageMetadataPkgVersion
    -- get the version record for the version being returned - the version will always be in this list ie. it will always be not empty
    let versionRecord = NE.head $ NE.fromList $ NE.filter (\v -> versionRecordNumber v == pkgVersion) pkgVersions
    manifest <-
        flip runReaderT settings $
            (snd <$> getManifest pkgId pkgVersion) >>= \bs ->
                runConduit $ bs .| CL.foldMap LBS.fromStrict
    icon <- loadIcon pkgId pkgVersion
    pure $
        PackageRes
            { packageResIcon = icon
            , packageResManifest = unsafeFromJust . decode $ manifest
            , packageResCategories = categoryName <$> pkgCategories
            , packageResInstructions = basicRender $ InstructionsR V0 pkgId
            , packageResLicense = basicRender $ LicenseR V0 pkgId
            , packageResVersions = versionRecordNumber <$> pkgVersions
            , packageResDependencies = dependencies
            , packageResPublishedAt = ((liftA2 fromMaybe) versionRecordCreatedAt versionRecordUpdatedAt) versionRecord
            }


loadIcon :: (MonadResource m, MonadReader r m, Has PkgRepo r) => PkgId -> Version -> m (ContentType, ByteString)
loadIcon pkg version = do
    (ct, _, src) <- getIcon pkg version
    buffered <- runConduit $ src .| CL.foldMap id
    pure (ct, buffered)


selectLatestVersionFromSpec ::
    (PkgId -> VersionRange) ->
    NonEmpty VersionRecord ->
    Maybe VersionRecord
selectLatestVersionFromSpec pkgRanges vs =
    let pkgId = NE.head $ versionRecordPkgId <$> vs
        spec = pkgRanges (unPkgRecordKey pkgId)
     in headMay . sortOn (Down . versionRecordNumber) $ NE.filter ((`satisfies` spec) . versionRecordNumber) vs
