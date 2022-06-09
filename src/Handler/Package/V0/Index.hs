{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Handler.Package.V0.Index where

import Conduit (concatMapC, dropC, mapC, mapMC, runConduit, sinkList, takeC, (.|))
import Control.Monad.Reader.Has (Functor (fmap), Has, Monad ((>>=)), MonadReader, ReaderT (runReaderT), ask, lift)
import Data.Aeson (FromJSON (..), ToJSON (..), Value, decode, eitherDecodeStrict, object, withObject, (.:), (.=))
import Data.Attoparsec.Text qualified as Atto
import Data.ByteString.Base64 (encodeBase64)
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
import Foundation (Handler, Route (InstructionsR, LicenseR))
import Handler.Types.Api (ApiVersion (..))
import Handler.Util (basicRender)
import Lib.Error (S9Error (..))
import Lib.PkgRepository (PkgRepo, getIcon, getManifest)
import Lib.Types.AppIndex (PkgId)
import Lib.Types.Emver (Version, VersionRange (..), parseRange, satisfies, (<||))
import Model (Category (..), Key (..), PkgDependency (..), VersionRecord (..))
import Network.HTTP.Types (status400)
import Protolude.Unsafe (unsafeFromJust)
import Settings (AppSettings)
import Startlude (
    Applicative ((*>)),
    Bifunctor (..),
    Bool (..),
    ByteString,
    ConvertText (toS),
    Down (..),
    Either (..),
    Eq (..),
    Generic,
    Int,
    Maybe (..),
    MonadIO,
    NonEmpty,
    Num ((*), (-)),
    Show,
    Text,
    Traversable (traverse),
    catMaybes,
    const,
    encodeUtf8,
    filter,
    flip,
    for,
    fromMaybe,
    headMay,
    id,
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
    (<>),
    (=<<),
 )
import UnliftIO (Concurrently (..), mapConcurrently)
import Yesod (
    MonadLogger,
    MonadResource,
    ToContent (..),
    ToTypedContent (..),
    YesodPersist (runDB),
    lookupGetParam,
    sendResponseStatus,
 )
import Yesod.Core (logWarn)


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


data PackageRes = PackageRes
    { packageResIcon :: !Text
    , packageResManifest :: !Value -- PackageManifest
    , packageResCategories :: ![Text]
    , packageResInstructions :: !Text
    , packageResLicense :: !Text
    , packageResVersions :: !(NonEmpty Version)
    , packageResDependencies :: !(HashMap PkgId DependencyRes)
    }
    deriving (Show, Generic)
instance ToJSON PackageRes where
    toJSON PackageRes{..} =
        object
            [ "icon" .= packageResIcon
            , "license" .= packageResLicense
            , "instructions" .= packageResInstructions
            , "manifest" .= packageResManifest
            , "categories" .= packageResCategories
            , "versions" .= packageResVersions
            , "dependency-metadata" .= packageResDependencies
            ]


newtype PackageListRes = PackageListRes [PackageRes]
    deriving (Generic)
instance ToJSON PackageListRes
instance ToContent PackageListRes where
    toContent = toContent . toJSON
instance ToTypedContent PackageListRes where
    toTypedContent = toTypedContent . toJSON


data DependencyRes = DependencyRes
    { dependencyResTitle :: !Text
    , dependencyResIcon :: !Text
    }
    deriving (Eq, Show)
instance ToJSON DependencyRes where
    toJSON DependencyRes{..} = object ["icon" .= dependencyResIcon, "title" .= dependencyResTitle]


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
    pkgIds <- getPkgIdsQuery
    category <- getCategoryQuery
    page <- fromMaybe 1 <$> getPageQuery
    limit' <- fromMaybe 20 <$> getLimitQuery
    query <- T.strip . fromMaybe "" <$> lookupGetParam "query"
    let (source, packageRanges) = case pkgIds of
            Nothing -> (serviceQuerySource category query, const Any)
            Just packages ->
                let s = getPkgDataSource (packageReqId <$> packages)
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


parseQueryParam :: Text -> (Text -> Either Text a) -> Handler (Maybe a)
parseQueryParam param parser = do
    lookupGetParam param >>= \case
        Nothing -> pure Nothing
        Just x -> case parser x of
            Left e -> do
                let err = InvalidParamsE ("get:" <> param) x
                $logWarn e
                sendResponseStatus status400 err
            Right a -> pure (Just a)


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
        pkgDepInfo <- getPkgDependencyData pkg pkgVersion
        pkgDepInfoWithVersions <- traverse getDependencyVersions pkgDepInfo
        let compatiblePkgDepInfo = fmap (filter (osPredicate . versionRecordOsVersion)) pkgDepInfoWithVersions
        let depMetadata = catMaybes $ zipWith selectDependencyBestVersion pkgDepInfo compatiblePkgDepInfo
        lift $
            fmap HM.fromList $
                for depMetadata $ \(depId, title, v) -> do
                    icon <- encodeBase64 <$> loadIcon depId v
                    pure $ (depId, DependencyRes title icon)


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
    manifest <-
        flip runReaderT settings $
            (snd <$> getManifest pkgId pkgVersion) >>= \bs ->
                runConduit $ bs .| CL.foldMap LBS.fromStrict
    icon <- loadIcon pkgId pkgVersion
    pure $
        PackageRes
            { packageResIcon = encodeBase64 icon -- pass through raw JSON Value, we have checked its correct parsing above
            , packageResManifest = unsafeFromJust . decode $ manifest
            , packageResCategories = categoryName <$> pkgCategories
            , packageResInstructions = basicRender $ InstructionsR V0 pkgId
            , packageResLicense = basicRender $ LicenseR V0 pkgId
            , packageResVersions = versionRecordNumber <$> pkgVersions
            , packageResDependencies = dependencies
            }


loadIcon :: (MonadResource m, MonadReader r m, Has PkgRepo r) => PkgId -> Version -> m ByteString
loadIcon pkg version = do
    (_, _, src) <- getIcon pkg version
    runConduit $ src .| CL.foldMap id


selectLatestVersionFromSpec ::
    (PkgId -> VersionRange) ->
    NonEmpty VersionRecord ->
    Maybe VersionRecord
selectLatestVersionFromSpec pkgRanges vs =
    let pkgId = NE.head $ versionRecordPkgId <$> vs
        spec = pkgRanges (unPkgRecordKey pkgId)
     in headMay . sortOn (Down . versionRecordNumber) $ NE.filter ((`satisfies` spec) . versionRecordNumber) vs


-- get best version of the dependency based on what is specified in the db (ie. what is specified in the manifest for the package)
selectDependencyBestVersion :: PkgDependency -> [VersionRecord] -> Maybe (PkgId, Text, Version)
selectDependencyBestVersion pkgDepRecord depVersions = do
    let depId = pkgDependencyDepId pkgDepRecord
    let versionRequirement = pkgDependencyDepVersionRange pkgDepRecord
    let satisfactory = filter ((<|| versionRequirement) . versionRecordNumber) depVersions
    case maximumOn versionRecordNumber satisfactory of
        Just bestVersion -> Just (unPkgRecordKey depId, versionRecordTitle bestVersion, versionRecordNumber bestVersion)
        Nothing -> Nothing
