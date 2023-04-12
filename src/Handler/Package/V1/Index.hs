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
import Database.Persist.Sql (SqlBackend)
import Database.Queries (
    collateVersions,
    getCategoriesFor,
    getDependencyVersions,
    getPkgDataSource,
    getPkgDependencyData,
    serviceQuerySource,
 )
import Foundation (Handler, Route (InstructionsR, LicenseR), RegistryCtx (appSettings))
import Handler.Package.Api (DependencyRes (..), PackageListRes (..), PackageRes (..))
import Handler.Types.Api (ApiVersion (..))
import Handler.Util (basicRender, parseQueryParam, getArchQuery, filterDeprecatedVersions)
import Lib.PkgRepository (PkgRepo, getIcon, getManifest)
import Lib.Types.Core (PkgId)
import Lib.Types.Emver (Version, VersionRange (..), parseRange, satisfies, (<||))
import Model (Category (..), Key (..), PkgDependency (..), VersionRecord (..), PkgRecord (pkgRecordHidden))
import Protolude.Unsafe (unsafeFromJust)
import Settings (AppSettings (minOsVersion))
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
    Traversable (traverse),
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
    maximumOn,
    nonEmpty,
    note,
    pure,
    readMaybe,
    snd,
    sortOn,
    zipWith,
    zipWithM,
    ($),
    (&&&),
    (.),
    (.*),
    (<$>),
    (<&>),
    (=<<),
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
import Database.Persist.Postgresql (entityVal)
import Yesod.Core (getsYesod)
import Data.List (head)

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
        getOsVersionQuery <&> \case
            Nothing -> const True
            Just v -> flip satisfies v
    osArch <- getArchQuery
    minOsVersion <- getsYesod $ minOsVersion . appSettings
    do
        pkgIds <- getPkgIdsQuery
        category <- getCategoryQuery
        page <- fromMaybe 1 <$> getPageQuery
        limit' <- fromMaybe 20 <$> getLimitQuery
        query <- T.strip . fromMaybe "" <$> lookupGetParam "query"
        let (source, packageRanges) = case pkgIds of
                Nothing -> (serviceQuerySource category query osArch, const Any)
                Just packages ->
                    let s = getPkgDataSource (packageReqId <$> packages) osArch
                        r = fromMaybe None . (flip lookup $ (packageReqId &&& packageReqVersion) <$> packages)
                    in (s, r)
        filteredPackages <-
            runDB $
                runConduit $
                    source
                        -- group conduit pipeline by pkg id
                        .| collateVersions
                        -- filter out versions of apps that are incompatible with the OS predicate
                        .| mapC (second (filter (osPredicate . versionRecordOsVersion)))
                        -- filter out deprecated service versions after a min os version
                        .| mapC (second (filterDeprecatedVersions minOsVersion osPredicate))                        
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
        pkgsWithDependencies <- runDB $ mapConcurrently (getPackageDependencies osPredicate) filteredPackages
        PackageListRes <$> runConcurrently (zipWithM (Concurrently .* constructPackageListApiRes) filteredPackages pkgsWithDependencies)

getPkgIdsQuery :: Handler (Maybe [PackageReq])
getPkgIdsQuery = parseQueryParam "ids" (first toS . eitherDecodeStrict . encodeUtf8)


getCategoryQuery :: Handler (Maybe Text)
getCategoryQuery = parseQueryParam "category" ((flip $ note . mappend "Invalid 'category': ") =<< (readMaybe . T.toUpper))


getPageQuery :: Handler (Maybe Int)
getPageQuery = parseQueryParam "page" ((flip $ note . mappend "Invalid 'page': ") =<< readMaybe)


getLimitQuery :: Handler (Maybe Int)
getLimitQuery = parseQueryParam "per-page" ((flip $ note . mappend "Invalid 'per-page': ") =<< readMaybe)


getOsVersionQuery :: Handler (Maybe VersionRange)
getOsVersionQuery = parseQueryParam "eos-version-compat" (first toS . Atto.parseOnly parseRange)


getPackageDependencies ::
    (MonadIO m, MonadLogger m, MonadResource m, Has PkgRepo r, MonadReader r m) =>
    (Version -> Bool) ->
    PackageMetadata ->
    ReaderT SqlBackend m (HashMap PkgId DependencyRes)
getPackageDependencies osPredicate PackageMetadata{packageMetadataPkgId = pkg, packageMetadataPkgVersion = pkgVersion} =
    do
        pkgDepInfo' <- getPkgDependencyData pkg pkgVersion
        let pkgDepInfo = fmap (\a -> (entityVal $ fst a, entityVal $ snd a)) pkgDepInfo'
        pkgDepInfoWithVersions <- traverse getDependencyVersions (fst <$> pkgDepInfo)
        let compatiblePkgDepInfo = fmap (filter (osPredicate . versionRecordOsVersion)) pkgDepInfoWithVersions
        let depMetadata = zipWith selectDependencyBestVersion pkgDepInfo compatiblePkgDepInfo
        lift $
            fmap HM.fromList $
                for depMetadata $ \(depId, title, v, isLocal) -> do
                    icon <- loadIcon depId v
                    pure $ (depId, DependencyRes title icon isLocal)


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


-- get best version of the dependency based on what is specified in the db (ie. what is specified in the manifest for the package)
selectDependencyBestVersion :: (PkgDependency, PkgRecord) -> [VersionRecord] -> (PkgId, Text, Version, Bool)
selectDependencyBestVersion pkgDepInfo depVersions = do
    let pkgDepRecord = fst pkgDepInfo
    let isLocal = pkgRecordHidden $ snd pkgDepInfo
    let depId = pkgDependencyDepId pkgDepRecord
    let versionRequirement = pkgDependencyDepVersionRange pkgDepRecord
    let satisfactory = filter ((<|| versionRequirement) . versionRecordNumber) depVersions
    let pkgId = unPkgRecordKey depId
    case maximumOn versionRecordNumber satisfactory of
        Just bestVersion -> (pkgId, versionRecordTitle bestVersion, versionRecordNumber bestVersion, isLocal)
        -- use latest version of dep for metadata info
        Nothing -> do
            let latestDepVersion = head $ sortOn (Down . versionRecordNumber) depVersions
            (pkgId, versionRecordTitle latestDepVersion, versionRecordNumber latestDepVersion, isLocal)
