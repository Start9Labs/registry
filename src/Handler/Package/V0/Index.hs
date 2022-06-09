{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Handler.Package.V0.Index where

import Conduit (runConduit, (.|))
import Control.Monad.Reader.Has (Functor (fmap), Has, Monad ((>>=)), MonadReader, ReaderT (runReaderT), ask)
import Data.Aeson (FromJSON (..), ToJSON (..), Value, decode, object, withObject, (.:), (.=))
import Data.Attoparsec.Text qualified as Atto
import Data.ByteString.Base64 (encodeBase64)
import Data.ByteString.Lazy qualified as LBS
import Data.Conduit.List qualified as CL
import Data.HashMap.Internal.Strict (HashMap)
import Data.HashMap.Strict qualified as HM
import Data.Text qualified as T
import Database.Marketplace (PackageMetadata (..), collateVersions, getPkgDependencyData, searchServices, zipDependencyVersions)
import Database.Persist (Entity (..), Key)
import Database.Persist.Sql (SqlBackend)
import Foundation (Handler, Route (InstructionsR, LicenseR))
import Lib.Error (S9Error (..))
import Lib.PkgRepository (PkgRepo, getIcon, getManifest)
import Lib.Types.AppIndex (PkgId)
import Lib.Types.Emver (Version, VersionRange, parseRange, satisfies)
import Model (Category (..), Key (..), PkgRecord (..), VersionRecord (..))
import Settings (AppSettings)
import Startlude (Bool (..), ByteString, Either (..), Eq, Generic, Int, Maybe (..), MonadIO, Read, Show, Text, Traversable (traverse), catMaybes, const, flip, fromMaybe, id, pure, snd, ($), (.), (<$>), (<&>))
import Yesod (MonadLogger, MonadResource, ToContent (..), ToTypedContent (..), YesodPersist (runDB), lookupGetParam)
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
    , packageResVersions :: ![Version]
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
instance FromJSON PackageRes where
    parseJSON = withObject "PackageRes" $ \o -> do
        packageResIcon <- o .: "icon"
        packageResLicense <- o .: "license"
        packageResInstructions <- o .: "instructions"
        packageResManifest <- o .: "manifest"
        packageResCategories <- o .: "categories"
        packageResVersions <- o .: "versions"
        packageResDependencies <- o .: "dependency-metadata"
        pure PackageRes{..}


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
instance FromJSON DependencyRes where
    parseJSON = withObject "DependencyRes" $ \o -> do
        dependencyResIcon <- o .: "icon"
        dependencyResTitle <- o .: "title"
        pure DependencyRes{..}


data PackageListDefaults = PackageListDefaults
    { packageListOrder :: !OrderArrangement
    , packageListPageLimit :: !Int -- the number of items per page
    , packageListPageNumber :: !Int -- the page you are on
    , packageListCategory :: !(Maybe Text)
    , packageListQuery :: !Text
    }
    deriving (Eq, Show, Read)
data OrderArrangement = ASC | DESC
    deriving (Eq, Show, Read)


getPackageListR :: Handler PackageListRes
getPackageListR = do
    osPredicate <-
        getOsVersionQuery <&> \case
            Nothing -> const True
            Just v -> flip satisfies v
    pkgIds <- getPkgIdsQuery
    filteredPackages <- case pkgIds of
        Nothing -> do
            -- query for all
            category <- getCategoryQuery
            page <- getPageQuery
            limit' <- getLimitQuery
            query <- T.strip . fromMaybe (packageListQuery defaults) <$> lookupGetParam "query"
            runDB $
                runConduit $
                    searchServices category query
                        .| collateVersions
                        .| zipCategories
                        -- empty list since there are no requested packages in this case
                        .| filterLatestVersionFromSpec []
                        .| filterPkgOsCompatible osPredicate
                        -- pages start at 1 for some reason. TODO: make pages start at 0
                        .| (dropC (limit' * (page - 1)) *> takeC limit')
                        .| sinkList
        Just packages' -> do
            -- for each item in list get best available from version range
            let vMap = (packageReqId &&& packageReqVersion) <$> packages'
            runDB
                -- TODO could probably be better with sequenceConduits
                . runConduit
                $ getPkgData (packageReqId <$> packages')
                    .| collateVersions
                    .| zipCategories
                    .| filterLatestVersionFromSpec vMap
                    .| filterPkgOsCompatible osPredicate
                    .| sinkList
    -- NOTE: if a package's dependencies do not meet the system requirements, it is currently omitted from the list
    pkgsWithDependencies <- runDB $ mapConcurrently (getPackageDependencies osPredicate) filteredPackages
    PackageListRes <$> mapConcurrently constructPackageListApiRes pkgsWithDependencies
    where
        defaults =
            PackageListDefaults
                { packageListOrder = DESC
                , packageListPageLimit = 20
                , packageListPageNumber = 1
                , packageListCategory = Nothing
                , packageListQuery = ""
                }
        getPkgIdsQuery :: Handler (Maybe [PackageReq])
        getPkgIdsQuery =
            lookupGetParam "ids" >>= \case
                Nothing -> pure Nothing
                Just ids -> case eitherDecodeStrict (encodeUtf8 ids) of
                    Left _ ->
                        do
                            let e = InvalidParamsE "get:ids" ids
                            $logWarn (show e) sendResponseStatus status400 e
                    Right a -> pure a
        getCategoryQuery :: Handler (Maybe Text)
        getCategoryQuery =
            lookupGetParam "category" >>= \case
                Nothing -> pure Nothing
                Just c -> case readMaybe . T.toUpper $ c of
                    Nothing ->
                        do
                            let e = InvalidParamsE "get:category" c
                            $logWarn (show e) sendResponseStatus status400 e
                    Just t -> pure $ Just t
        getPageQuery :: Handler Int
        getPageQuery =
            lookupGetParam "page" >>= \case
                Nothing -> pure $ packageListPageNumber defaults
                Just p -> case readMaybe p of
                    Nothing ->
                        do
                            let e = InvalidParamsE "get:page" p
                            $logWarn (show e) sendResponseStatus status400 e
                    Just t -> pure $ case t of
                        0 -> 1 -- disallow page 0 so offset is not negative
                        _ -> t
        getLimitQuery :: Handler Int
        getLimitQuery =
            lookupGetParam "per-page" >>= \case
                Nothing -> pure $ packageListPageLimit defaults
                Just pp -> case readMaybe pp of
                    Nothing ->
                        do
                            let e = InvalidParamsE "get:per-page" pp
                            $logWarn (show e) sendResponseStatus status400 e
                    Just l -> pure l
        getOsVersionQuery :: Handler (Maybe VersionRange)
        getOsVersionQuery =
            lookupGetParam "eos-version-compat" >>= \case
                Nothing -> pure Nothing
                Just osv -> case Atto.parseOnly parseRange osv of
                    Left _ ->
                        do
                            let e = InvalidParamsE "get:eos-version-compat" osv
                            $logWarn (show e) sendResponseStatus status400 e
                    Right v -> pure $ Just v
        getPackageDependencies ::
            (MonadIO m, MonadLogger m) =>
            (Version -> Bool) ->
            PackageMetadata ->
            ReaderT
                SqlBackend
                m
                ( Key PkgRecord
                , [Category]
                , [Version]
                , Version
                , [(Key PkgRecord, Text, Version)]
                )
        getPackageDependencies osPredicate PackageMetadata{packageMetadataPkgId = pkg, packageMetadataPkgVersionRecords = pkgVersions, packageMetadataPkgCategories = pkgCategories, packageMetadataPkgVersion = pkgVersion} =
            do
                let pkgId = PkgRecordKey pkg
                let pkgVersions' = versionRecordNumber . entityVal <$> pkgVersions
                let pkgCategories' = entityVal <$> pkgCategories
                pkgDepInfo <- getPkgDependencyData pkgId pkgVersion
                pkgDepInfoWithVersions <- traverse zipDependencyVersions pkgDepInfo
                let compatiblePkgDepInfo = fmap (filterDependencyOsCompatible osPredicate) pkgDepInfoWithVersions
                res <- catMaybes <$> traverse filterDependencyBestVersion compatiblePkgDepInfo
                pure (pkgId, pkgCategories', pkgVersions', pkgVersion, res)
        constructPackageListApiRes ::
            (MonadResource m, MonadReader r m, Has AppSettings r, Has PkgRepo r) =>
            ( Key PkgRecord
            , [Category]
            , [Version]
            , Version
            , [(Key PkgRecord, Text, Version)]
            ) ->
            m PackageRes
        constructPackageListApiRes (pkgKey, pkgCategories, pkgVersions, pkgVersion, dependencies) = do
            settings <- ask @_ @_ @AppSettings
            let pkgId = unPkgRecordKey pkgKey
            manifest <-
                flip runReaderT settings $
                    (snd <$> getManifest pkgId pkgVersion) >>= \bs ->
                        runConduit $ bs .| CL.foldMap LBS.fromStrict
            icon <- loadIcon pkgId pkgVersion
            deps <- constructDependenciesApiRes dependencies
            pure $
                PackageRes
                    { packageResIcon = encodeBase64 icon -- pass through raw JSON Value, we have checked its correct parsing above
                    , packageResManifest = unsafeFromJust . decode $ manifest
                    , packageResCategories = categoryName <$> pkgCategories
                    , packageResInstructions = basicRender $ InstructionsR _ pkgId
                    , packageResLicense = basicRender $ LicenseR _ pkgId
                    , packageResVersions = pkgVersions
                    , packageResDependencies = HM.fromList deps
                    }
        constructDependenciesApiRes ::
            (MonadResource m, MonadReader r m, Has PkgRepo r) =>
            [(Key PkgRecord, Text, Version)] ->
            m [(PkgId, DependencyRes)]
        constructDependenciesApiRes deps =
            traverse
                ( \(depKey, depTitle, depVersion) -> do
                    let depId = unPkgRecordKey depKey
                    icon <- loadIcon depId depVersion
                    pure (depId, DependencyRes{dependencyResTitle = depTitle, dependencyResIcon = encodeBase64 icon})
                )
                deps
        loadIcon :: (MonadResource m, MonadReader r m, Has PkgRepo r) => PkgId -> Version -> m ByteString
        loadIcon pkg version = do
            (_, _, src) <- getIcon pkg version
            runConduit $ src .| CL.foldMap id
