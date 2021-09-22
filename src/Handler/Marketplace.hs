{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TemplateHaskell  #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveAnyClass #-}

module Handler.Marketplace where
<<<<<<< HEAD
import           Startlude               hiding ( from
                                                , Handler
                                                , on
                                                , sortOn
                                                )
import           Foundation
import           Yesod.Core
import qualified Database.Persist              as P
import           Model
import           Yesod.Persist.Core
import           Database.Marketplace
import           Data.List
import           Lib.Types.Category
import           Lib.Types.AppIndex
import qualified Data.HashMap.Strict           as HM
import           Lib.Types.Emver
import qualified Data.List.NonEmpty            as NE
import           Database.Esqueleto.Experimental
import           Lib.Error
import           Network.HTTP.Types
import           Lib.Registry
import           Settings
import           System.FilePath.Posix
import           Lib.External.AppMgr
import           Data.Aeson
import qualified Data.ByteString.Lazy          as BS
import qualified Data.Text                     as T
import           Data.String.Interpolate.IsString
import           Util.Shared
=======
import Startlude hiding (from, Handler, on, sortOn)
import Foundation
import Yesod.Core
import qualified Database.Persist as P
import Model
import Yesod.Persist.Core
import Database.Marketplace
import Data.List
import Lib.Types.Category
import Lib.Types.AppIndex
import qualified Data.HashMap.Strict as HM
import Lib.Types.Emver
import qualified Data.List.NonEmpty as NE
import Database.Esqueleto.Experimental
import Lib.Error
import Network.HTTP.Types
import Lib.Registry
import Settings
import System.FilePath.Posix
import Lib.External.AppMgr
import Data.Aeson
import qualified Data.ByteString.Lazy     as BS
import qualified Data.Text as T
import Data.String.Interpolate.IsString
import Util.Shared
import Lib.Types.AppIndex()
import  UnliftIO.Async
import qualified Database.PostgreSQL.Simple as PS
import qualified Database.Persist.Postgresql as PP
import Database.PostgreSQL.Simple (FromRow)
import Database.PostgreSQL.Simple.FromRow (FromRow(fromRow), field)
import Database.Esqueleto.PostgreSQL (arrayAggDistinct)
>>>>>>> aggregate query functions

newtype CategoryRes = CategoryRes {
    categories :: [CategoryTitle]
} deriving (Show, Generic)
instance ToJSON CategoryRes
instance FromJSON CategoryRes
instance ToContent CategoryRes where
    toContent = toContent . toJSON
instance ToTypedContent CategoryRes where
    toTypedContent = toTypedContent . toJSON
data ServiceRes =  ServiceRes
    { serviceResIcon :: URL
    , serviceResManifest :: Maybe Data.Aeson.Value -- ServiceManifest
    , serviceResCategories :: [CategoryTitle]
    , serviceResInstructions :: URL
    , serviceResLicense :: URL
    , serviceResVersions :: [Version]
    , serviceResDependencyInfo :: HM.HashMap AppIdentifier DependencyInfo
    } deriving (Generic)

newtype ReleaseNotes = ReleaseNotes { unReleaseNotes :: HM.HashMap Version Text }
    deriving (Eq, Show)
instance ToJSON ReleaseNotes where
    toJSON ReleaseNotes {..} = object [ t .= v | (k, v) <- HM.toList unReleaseNotes, let (String t) = toJSON k ]
instance ToContent ReleaseNotes where
    toContent = toContent . toJSON
instance ToTypedContent ReleaseNotes where
    toTypedContent = toTypedContent . toJSON
instance ToJSON ServiceRes where
    toJSON ServiceRes {..} = object
        [ "icon" .= serviceResIcon
        , "license" .= serviceResLicense
        , "instructions" .= serviceResInstructions
        , "manifest" .= serviceResManifest
        , "categories" .= serviceResCategories
        , "versions" .= serviceResVersions
        , "dependency-metadata" .= serviceResDependencyInfo
        ]
instance ToContent ServiceRes where
    toContent = toContent . toJSON
instance ToTypedContent ServiceRes where
    toTypedContent = toTypedContent . toJSON
data DependencyInfo = DependencyInfo
    { dependencyInfoTitle :: AppIdentifier
    , dependencyInfoIcon :: URL
    } deriving (Eq, Show)
instance ToJSON DependencyInfo where
    toJSON DependencyInfo {..} = object ["icon" .= dependencyInfoIcon, "title" .= dependencyInfoTitle]

data ServiceListRes = ServiceListRes {
    serviceListResCategories :: [CategoryTitle]
    , serviceListResServices :: [ServiceAvailable]
} deriving (Show)
instance ToJSON ServiceListRes where
    toJSON ServiceListRes {..} =
        object ["categories" .= serviceListResCategories, "services" .= serviceListResServices]
instance ToContent ServiceListRes where
    toContent = toContent . toJSON
instance ToTypedContent ServiceListRes where
    toTypedContent = toTypedContent . toJSON

data ServiceAvailable = ServiceAvailable
    { serviceAvailableId :: AppIdentifier
    , serviceAvailableTitle :: Text
    , serviceAvailableVersion :: Version
    , serviceAvailableIcon :: URL
    , serviceAvailableDescShort :: Text
    } deriving (Show)
instance ToJSON ServiceAvailable where
    toJSON ServiceAvailable {..} = object
        [ "id" .= serviceAvailableId
        , "title" .= serviceAvailableTitle
        , "version" .= serviceAvailableVersion
        , "icon" .= serviceAvailableIcon
        , "descriptionShort" .= serviceAvailableDescShort
        ]
instance ToContent ServiceAvailable where
    toContent = toContent . toJSON
instance ToTypedContent ServiceAvailable where
    toTypedContent = toTypedContent . toJSON

newtype ServiceAvailableRes = ServiceAvailableRes [ServiceRes]
    deriving (Generic)
instance ToJSON ServiceAvailableRes
instance ToContent ServiceAvailableRes where
    toContent = toContent . toJSON
instance ToTypedContent ServiceAvailableRes where
    toTypedContent = toTypedContent . toJSON

newtype VersionLatestRes = VersionLatestRes (HM.HashMap AppIdentifier (Maybe Version))
    deriving (Show, Generic)
instance ToJSON VersionLatestRes
instance ToContent VersionLatestRes where
    toContent = toContent . toJSON
instance ToTypedContent VersionLatestRes where
    toTypedContent = toTypedContent . toJSON
data OrderArrangement = ASC | DESC
    deriving (Eq, Show, Read)
data ServiceListDefaults = ServiceListDefaults
    { serviceListOrder :: OrderArrangement
    , serviceListPageLimit :: Int64 -- the number of items per page
    , serviceListPageNumber :: Int64 -- the page you are on
    , serviceListCategory :: Maybe CategoryTitle
    , serviceListQuery :: Text
    }
    deriving (Eq, Show, Read)
data EosRes = EosRes
    { eosResVersion :: Version
    , eosResHeadline :: Text
    , eosResReleaseNotes :: ReleaseNotes
} deriving (Eq, Show, Generic)
instance ToJSON EosRes where
<<<<<<< HEAD
    toJSON EosRes {..} =
        object ["version" .= eosResVersion, "headline" .= eosResHeadline, "release-notes" .= eosResReleaseNotes]
=======
    toJSON EosRes { .. } = object
        [ "version" .= eosResVersion
        , "headline" .= eosResHeadline
        , "release-notes" .= eosResReleaseNotes
        ]
>>>>>>> aggregate query functions
instance ToContent EosRes where
    toContent = toContent . toJSON
instance ToTypedContent EosRes where
    toTypedContent = toTypedContent . toJSON

data PackageVersion = PackageVersion
    { packageVersionId :: AppIdentifier
    , packageVersionVersion :: VersionRange
    } deriving (Show)
instance FromJSON PackageVersion where
    parseJSON = withObject "package version" $ \o -> do
        packageVersionId      <- o .: "id"
        packageVersionVersion <- o .: "version"
        pure PackageVersion { .. }

getCategoriesR :: Handler CategoryRes
getCategoriesR = do
    allCategories <- runDB $ select $ do
        cats <- from $ table @Category
        orderBy [desc (cats ^. CategoryPriority)]
        pure cats
    pure $ CategoryRes $ categoryName . entityVal <$> allCategories

getEosR :: Handler EosRes
getEosR = do
    allEosVersions <- runDB $ select $ do
        vers <- from $ table @OsVersion
        orderBy [desc (vers ^. OsVersionCreatedAt)]
        pure vers
    let osV    = entityVal <$> allEosVersions
    let latest = Data.List.head osV
    let mappedVersions =
            ReleaseNotes
                $   HM.fromList
                $   sortOn (Down . fst)
                $   (\v -> (osVersionNumber v, osVersionReleaseNotes v))
                <$> osV
    pure $ EosRes { eosResVersion      = osVersionNumber latest
                  , eosResHeadline     = osVersionHeadline latest
                  , eosResReleaseNotes = mappedVersions
                  }

getReleaseNotesR :: Handler ReleaseNotes
getReleaseNotesR = do
    getParameters <- reqGetParams <$> getRequest
    case lookup "id" getParameters of
        Nothing      -> sendResponseStatus status400 ("expected query param \"id\" to exist" :: Text)
        Just package -> do
<<<<<<< HEAD
            (service, _             ) <- runDB $ fetchLatestApp package >>= errOnNothing status404 "package not found"
            (_      , mappedVersions) <- fetchAllAppVersions (entityKey service)
=======
            (service, _) <- runDB $ fetchLatestApp (AppIdentifier package) >>= errOnNothing status404 "package not found"
            (_, mappedVersions) <- fetchAllAppVersions (entityKey service)
>>>>>>> aggregate query functions
            pure mappedVersions

getVersionLatestR :: Handler VersionLatestRes
getVersionLatestR = do
    getParameters <- reqGetParams <$> getRequest
    case lookup "ids" getParameters of
        Nothing       -> sendResponseStatus status400 ("expected query param \"ids\" to exist" :: Text)
        Just packages -> case eitherDecode $ BS.fromStrict $ encodeUtf8 packages of
<<<<<<< HEAD
            Left e -> sendResponseStatus status400 ("could not parse query param \"ids\"" <> show e :: Text)
            Right (p :: [AppIdentifier]) -> do
                let packageList :: [(AppIdentifier, Maybe Version)] = (, Nothing) <$> p
                found <- runDB $ traverse fetchLatestApp $ fst <$> packageList
                pure
                    $ VersionLatestRes
                    $ HM.union
                          (   HM.fromList
                          $   (\v ->
                                  ( sAppAppId $ entityVal $ fst v :: AppIdentifier
                                  , Just $ sVersionNumber $ entityVal $ snd v
                                  )
                              )
                          <$> catMaybes found
                          )
                    $ HM.fromList packageList
=======
                Left e -> sendResponseStatus status400 ("could not parse query param \"ids\"" <> show e :: Text)
                Right (p :: [AppIdentifier])-> do
                    let packageList :: [(AppIdentifier, Maybe Version)] =  (, Nothing) <$> p
                    found <- runDB $ traverse fetchLatestApp $ fst <$> packageList
                    pure $ VersionLatestRes $ HM.union (HM.fromList $ (\v -> (sAppAppId $ entityVal $ fst v, Just $ sVersionNumber $ entityVal $ snd v)) <$> catMaybes found) $ HM.fromList packageList
>>>>>>> aggregate query functions

getPackageListR :: Handler ServiceAvailableRes
getPackageListR = do
    getParameters <- reqGetParams <$> getRequest
<<<<<<< HEAD
    let defaults = ServiceListDefaults { serviceListOrder      = DESC
                                       , serviceListPageLimit  = 20
                                       , serviceListPageNumber = 1
                                       , serviceListCategory   = Nothing
                                       , serviceListQuery      = ""
                                       }
=======
    let defaults = ServiceListDefaults
            { serviceListOrder = DESC
            , serviceListPageLimit = 20
            , serviceListPageNumber = 1
            , serviceListCategory = Nothing
            , serviceListQuery = ""
            }
>>>>>>> aggregate query functions
    case lookup "ids" getParameters of
        Nothing -> do
            -- query for all
            category <- case lookup "category" getParameters of
                Nothing -> pure $ serviceListCategory defaults
                Just c  -> case readMaybe $ T.toUpper c of
                    Nothing -> do
                        $logInfo c
                        sendResponseStatus status400 ("could not read category" :: Text)
                    Just t -> pure $ Just t
            page <- case lookup "page" getParameters of
                Nothing -> pure $ serviceListPageNumber defaults
                Just p  -> case readMaybe p of
                    Nothing -> do
                        $logInfo p
                        sendResponseStatus status400 ("could not read page" :: Text)
                    Just t -> pure $ case t of
                        0 -> 1 -- disallow page 0 so offset is not negative
                        _ -> t
            limit' <- case lookup "per-page" getParameters of
                Nothing -> pure $ serviceListPageLimit defaults
                Just c  -> case readMaybe $ toS c of
                    Nothing -> sendResponseStatus status400 ("could not read per-page" :: Text)
                    Just l  -> pure l
            query <- T.filter (not . isSpace) . fromMaybe (serviceListQuery defaults) <$> lookupGetParam "query"
            filteredServices <- runDB $ searchServices category limit' ((page - 1) * limit') query
            let filteredServices' = sAppAppId . entityVal <$> filteredServices
            settings <- getsYesod appSettings
            packageMetadata <- runDB $ fetchPackageMetadata filteredServices'
            $logInfo $ show packageMetadata
            serviceDetailResult <- liftIO $ mapConcurrently (getServiceDetails settings packageMetadata Nothing) filteredServices'
            let (errors, services) = partitionEithers serviceDetailResult
            pure $ ServiceAvailableRes services
            -- if null errors
            --     then pure $ ServiceAvailableRes services
            --     else sendResponseStatus status500 ("Errors acquiring service details: " <> show <$> errors)

        Just packageVersionList -> case eitherDecode $ BS.fromStrict $ encodeUtf8 packageVersionList of
<<<<<<< HEAD
            Left e -> sendResponseStatus status400 ("could not parse query param \"ids\"" <> show e :: Text)
            Right (packages :: [PackageVersion]) -> do
                -- for each item in list get best available from version range
                availableServices <- traverse getPackageDetails packages
                services          <- traverse (uncurry getServiceDetails) availableServices
                pure $ ServiceAvailableRes services
            where
                getPackageDetails :: PackageVersion -> HandlerFor RegistryCtx (Maybe (Entity SVersion), Entity SApp)
                getPackageDetails pv = do
                    appsDir <- getsYesod $ ((</> "apps") . resourcesDir) . appSettings
                    let appId  = packageVersionId pv
                    let spec   = packageVersionVersion pv
                    let appExt = Extension (toS appId) :: Extension "s9pk"
                    getBestVersion appsDir appExt spec >>= \case
                        Nothing -> sendResponseStatus
                            status404
                            ("best version could not be found for " <> appId <> " with spec " <> show spec :: Text)
                        Just v -> do
                            (service, version) <- runDB $ fetchLatestAppAtVersion appId v >>= errOnNothing
                                status404
                                ("service at version " <> show v <> " not found")
                            pure (Just version, service)

getServiceR :: Handler ServiceRes
getServiceR = do
    getParameters      <- reqGetParams <$> getRequest
    (service, version) <- case lookup "id" getParameters of
        Nothing     -> sendResponseStatus status404 ("id param should exist" :: Text)
        Just appId' -> do
            case lookup "version" getParameters of
                -- default to latest - @TODO need to determine best available based on OS version?
                Nothing -> runDB $ fetchLatestApp appId' >>= errOnNothing status404 "service not found"
                Just v  -> do
                    case readMaybe v of
                        Nothing -> sendResponseStatus status400 ("Invalid App Version Specification" :: Text)
                        Just vv -> runDB $ fetchLatestAppAtVersion appId' vv >>= errOnNothing
                            status404
                            ("service at version " <> show v <> " not found")
    getServiceDetails (Just version) service

getServiceDetails :: Maybe (Entity SVersion) -> Entity SApp -> HandlerFor RegistryCtx ServiceRes
getServiceDetails maybeVersion service = do
    (versions, _)        <- fetchAllAppVersions (entityKey service)
    categories           <- runDB $ fetchAppCategories (entityKey service)
    (appsDir, appMgrDir) <- getsYesod $ ((</> "apps") . resourcesDir &&& staticBinDir) . appSettings
    domain               <- getsYesod $ registryHostname . appSettings
    let appId = sAppAppId $ entityVal service
    version <- case maybeVersion of
        Nothing -> do
            (_, version) <- runDB $ fetchLatestApp appId >>= errOnNothing status404 "service not found"
            pure $ sVersionNumber $ entityVal version
        Just v -> pure $ sVersionNumber $ entityVal v
    let appDir = (<> "/") . (</> show version) . (</> toS appId) $ appsDir
    let appExt = Extension (toS appId) :: Extension "s9pk"
    manifest' <- handleS9ErrT $ getManifest appMgrDir appDir appExt
    manifest  <- case eitherDecode $ BS.fromStrict manifest' of
        Left e -> do
            $logError "could not parse service manifest!"
            $logError (show e)
            sendResponseStatus status500 ("Internal Server Error" :: Text)
        Right a -> pure a
    d <- traverse (mapDependencyMetadata appsDir domain) (HM.toList $ serviceManifestDependencies manifest)
    pure $ ServiceRes { serviceResIcon           = [i|https://#{domain}/package/icon/#{appId}|]
                      , serviceResManifest       = decode $ BS.fromStrict manifest' -- pass through raw JSON Value
                      , serviceResCategories     = serviceCategoryCategoryName . entityVal <$> categories
                      , serviceResInstructions   = [i|https://#{domain}/package/instructions/#{appId}|]
                      , serviceResLicense        = [i|https://#{domain}/package/license/#{appId}|]
                      , serviceResVersions       = versionInfoVersion <$> versions
                      , serviceResDependencyInfo = HM.fromList d
                      }

type URL = Text
mapDependencyMetadata :: (MonadIO m, MonadHandler m)
                      => FilePath
                      -> Text
                      -> (AppIdentifier, ServiceDependencyInfo)
                      -> m (AppIdentifier, DependencyInfo)
=======
                        Left e -> sendResponseStatus status400 ("could not parse query param \"ids\"" <> show e :: Text)
                        Right (packages :: [PackageVersion])-> do
                            -- for each item in list get best available from version range
                            settings <- getsYesod appSettings
                            availableServices <- traverse (getPackageDetails settings) packages
                            packageMetadata <- runDB $ fetchPackageMetadata (snd <$> availableServices)
                            serviceDetailResult <- liftIO $ mapConcurrently (uncurry $ getServiceDetails settings packageMetadata) availableServices
                            let (errors, services) = partitionEithers serviceDetailResult
                            pure $ ServiceAvailableRes services
                            -- if null errors
                            --     then pure $ ServiceAvailableRes services
                            --     else sendResponseStatus status500 ("Errors acquiring service details: " <> show <$> errors)
            where
                getPackageDetails :: (MonadHandler m) => AppSettings -> PackageVersion -> m (Maybe Version, AppIdentifier)
                getPackageDetails settings pv = do
                    let appId = packageVersionId pv
                    let spec = packageVersionVersion pv
                    let appExt = Extension (show appId) :: Extension "s9pk"
                    getBestVersion ((</> "apps") . resourcesDir $ settings) appExt spec >>= \case
                        Nothing -> sendResponseStatus status404 ("best version could not be found for " <> show appId <> " with spec " <> show spec :: Text)
                        Just v -> do
                            pure (Just v, appId)

getServiceDetails :: (MonadIO m, Monad m, MonadError IOException  m) => AppSettings -> (HM.HashMap AppIdentifier ([Version], [CategoryTitle])) -> Maybe Version -> AppIdentifier -> m (Either Text ServiceRes)
getServiceDetails settings metadata maybeVersion appId = do
    packageMetadata <- case HM.lookup appId metadata of
            Nothing-> throwIO $ NotFoundE [i|#{appId} not found.|]
            Just m -> pure m
    let (appsDir, appMgrDir) = ((</> "apps") . resourcesDir &&& staticBinDir) settings
    let domain = registryHostname settings
    version <- case maybeVersion of
            Nothing -> do
                -- grab first value, which will be the latest version
                case fst packageMetadata of
                    [] -> throwIO $ NotFoundE $ "no latest version found for " <> show appId
                    x:_ -> pure x
            Just v -> pure v
    let appDir = (<> "/") . (</> show version) . (</> show appId) $ appsDir
    let appExt = Extension (show appId) :: Extension "s9pk"
    manifest' <- handleS9ErrNuclear $ getManifest appMgrDir appDir appExt
    case eitherDecode $ BS.fromStrict manifest' of
            Left e -> pure $ Left $ "Could not parse service manifest for " <> show appId <> ": " <> show e
            Right m -> do
                    d <- liftIO $ mapConcurrently (mapDependencyMetadata appsDir domain) (HM.toList $ serviceManifestDependencies m)
                    pure $ Right $ ServiceRes
                        { serviceResIcon = [i|https://#{domain}/package/icon/#{appId}|]
                        , serviceResManifest = decode $ BS.fromStrict manifest' -- pass through raw JSON Value
                        , serviceResCategories = snd packageMetadata
                        , serviceResInstructions = [i|https://#{domain}/package/instructions/#{appId}|]
                        , serviceResLicense = [i|https://#{domain}/package/license/#{appId}|]
                        , serviceResVersions = fst packageMetadata
                        , serviceResDependencyInfo = HM.fromList $ snd $ partitionEithers d
                        }


type URL = Text
mapDependencyMetadata :: (MonadIO m) => FilePath -> Text -> (AppIdentifier, ServiceDependencyInfo) -> m (Either Text (AppIdentifier, DependencyInfo))
>>>>>>> aggregate query functions
mapDependencyMetadata appsDir domain (appId, depInfo) = do
    let ext = (Extension (show appId) :: Extension "s9pk")
    -- get best version from VersionRange of dependency
    version <- getBestVersion appsDir ext (serviceDependencyInfoVersion depInfo) >>= \case
<<<<<<< HEAD
        Nothing -> sendResponseStatus status404 ("best version not found for dependent package " <> appId :: Text)
        Just v  -> pure v
    pure
        ( appId
        , DependencyInfo { dependencyInfoTitle = appId
                         , dependencyInfoIcon  = [i|https://#{domain}/package/icon/#{appId}?spec==#{version}|]
                         }
        )
=======
        Nothing -> throwIO $ NotFoundE $ "best version not found for dependent package " <> show appId
        Just v -> pure v
    pure $ Right (appId, DependencyInfo
            { dependencyInfoTitle = appId
            , dependencyInfoIcon = [i|https://#{domain}/package/icon/#{appId}?spec==#{version}|]
            })
>>>>>>> aggregate query functions

decodeIcon :: (MonadHandler m, KnownSymbol a) => FilePath -> FilePath -> Extension a -> m URL
decodeIcon appmgrPath depPath e@(Extension icon) = do
    icon' <- handleS9ErrT $ getIcon appmgrPath depPath e
    case eitherDecode $ BS.fromStrict icon' of
        Left e' -> do
            $logInfo $ T.pack e'
            sendResponseStatus status400 e'
        Right (i' :: URL) -> pure $ i' <> T.pack icon

decodeInstructions :: (MonadHandler m, KnownSymbol a) => FilePath -> FilePath -> Extension a -> m Text
decodeInstructions appmgrPath depPath package = do
    instructions <- handleS9ErrT $ getInstructions appmgrPath depPath package
    pure $ decodeUtf8 instructions

decodeLicense :: (MonadHandler m, KnownSymbol a) => FilePath -> FilePath -> Extension a -> m Text
decodeLicense appmgrPath depPath package = do
    license <- handleS9ErrT $ getLicense appmgrPath depPath package
    pure $ decodeUtf8 license

fetchAllAppVersions :: Key SApp -> HandlerFor RegistryCtx ([VersionInfo], ReleaseNotes)
fetchAllAppVersions appId = do
    entityAppVersions <- runDB $ P.selectList [SVersionAppId P.==. appId] [] -- orderby version
    let vers           = entityVal <$> entityAppVersions
    let vv             = mapSVersionToVersionInfo vers
    let mappedVersions = ReleaseNotes $ HM.fromList $ (\v -> (versionInfoVersion v, versionInfoReleaseNotes v)) <$> vv
    pure (vv, mappedVersions)
    where
        mapSVersionToVersionInfo :: [SVersion] -> [VersionInfo]
        mapSVersionToVersionInfo sv = do
            (\v -> VersionInfo {
            versionInfoVersion = sVersionNumber v
            , versionInfoReleaseNotes = sVersionReleaseNotes v
            , versionInfoDependencies = HM.empty
            , versionInfoOsRequired = sVersionOsVersionRequired v
            , versionInfoOsRecommended = sVersionOsVersionRecommended v
            , versionInfoInstallAlert = Nothing
            }) <$> sv

fetchMostRecentAppVersions :: MonadIO m => Key SApp -> ReaderT SqlBackend m [Entity SVersion]
fetchMostRecentAppVersions appId = select $ do
    version <- from $ table @SVersion
    where_ (version ^. SVersionAppId ==. val appId)
    orderBy [desc (version ^. SVersionNumber)]
    limit 1
    pure version

fetchLatestApp :: MonadIO m => AppIdentifier -> ReaderT SqlBackend m (Maybe (P.Entity SApp, P.Entity SVersion))
fetchLatestApp appId = selectOne $ do
<<<<<<< HEAD
    (service :& version) <-
        from
        $           table @SApp
        `innerJoin` table @SVersion
        `on`        (\(service :& version) -> service ^. SAppId ==. version ^. SVersionAppId)
    where_ (service ^. SAppAppId ==. val appId)
    orderBy [desc (version ^. SVersionNumber)]
    pure (service, version)

fetchLatestAppAtVersion :: MonadIO m
                        => Text
                        -> Version
                        -> ReaderT SqlBackend m (Maybe (P.Entity SApp, P.Entity SVersion))
=======
                            (service :& version) <-
                                from $ table @SApp
                                `innerJoin` table @SVersion
                                `on` (\(service :& version) ->
                                        service ^. SAppId ==. version ^. SVersionAppId)
                            where_ (service ^. SAppAppId ==. val appId)
                            orderBy [ desc (version ^. SVersionNumber)]
                            pure (service, version)

fetchLatestAppAtVersion :: MonadIO m => AppIdentifier -> Version -> ReaderT SqlBackend m (Maybe (P.Entity SApp, P.Entity SVersion))
>>>>>>> aggregate query functions
fetchLatestAppAtVersion appId version' = selectOne $ do
    (service :& version) <-
        from
        $           table @SApp
        `innerJoin` table @SVersion
        `on`        (\(service :& version) -> service ^. SAppId ==. version ^. SVersionAppId)
    where_ $ (service ^. SAppAppId ==. val appId) &&. (version ^. SVersionNumber ==. val version')
    pure (service, version)

data PackageMetadata = PackageMetadata
    { packageMetadataId :: AppIdentifier
    , packageMetadataVersions :: [Version]
    , packageMetadataCategories :: [CategoryTitle]
    } deriving (Eq, Show, Generic)
instance RawSql PackageMetadata where
     rawSqlCols _ _ = (3, [])
     rawSqlColCountReason _ = "because that is the number of fields in the data type"
     rawSqlProcessRow pv = case pv of
            [] -> Left "empty row"
            _:xs -> Right $ PackageMetadata
                { packageMetadataId = case fromPersistValue $ xs !! 1 of
                    Left _ -> ""
                    Right v -> v
                , packageMetadataVersions = case fromPersistValue $ xs !! 2 of
                    Left _ -> []
                    Right v -> v
                , packageMetadataCategories = case fromPersistValue $ xs !! 3 of
                    Left _ -> []
                    Right v -> v
                }
-- instance FromJSON PackageMetadata where
--     parseJSON = withObject "package data" $ \o -> do
--         packageMetadataId <- o .: "app_id"
--         packageMetadataVersions <- o .: "versions"
--         packageMetadataCategories <- o .: "categories"
--         pure PackageMetadata { .. }
-- instance ToJSON PackageMetadata where
--     toJSON PackageMetadata {..} = object
--         [ "app_id" .= packageMetadataId
--         , "versions" .= packageMetadataVersions
--         , "categories" .= packageMetadataCategories
--         ]
-- instance PersistField PackageMetadata where
--     fromPersistValue = fromPersistValueJSON
--     toPersistValue = toPersistValueJSON
-- instance FromRow PackageMetadata where
--     fromRow = PackageMetadata <$> field <*> (fmap Version <$> field) <*> (fmap parseCT <$> field)

fetchPackageMetadataX :: MonadIO m => [AppIdentifier] -> ReaderT SqlBackend m [PackageMetadata]
fetchPackageMetadataX ids = rawSql "SELECT s.app_id, json_agg(DISTINCT v.number ORDER BY v.number DESC) AS versions, json_agg(DISTINCT c.category_name) AS categories FROM s_app s LEFT JOIN service_category c on s.id = c.service_id JOIN version v on v.app_id = s.id WHERE s.app_id IN (?) GROUP BY s.app_id" [PersistList (toPersistValue <$> ids)]

fetchPackageMetadata :: MonadUnliftIO m => [AppIdentifier] -> ReaderT SqlBackend m (HM.HashMap AppIdentifier ([Version], [CategoryTitle]))
fetchPackageMetadata ids = do
    let categoriesQuery = select $ do
            (service :& category) <- from $ table @SApp 
                `leftJoin` table @ServiceCategory 
                `on` (\(service :& category) -> Database.Esqueleto.Experimental.just (service ^. SAppId) ==. category ?. ServiceCategoryServiceId)
            where_ $ 
                service ^. SAppAppId `in_` valList ids
            Database.Esqueleto.Experimental.groupBy $ service ^. SAppAppId
            pure (service ^. SAppAppId, arrayAggDistinct (category ?. ServiceCategoryCategoryName))
    let versionsQuery = select $ do
            (service :& version) <- from $ table @SApp 
                `innerJoin` table @SVersion 
                    `on` (\(service :& version) -> (service ^. SAppId) ==. version ^. SVersionAppId)
            where_ $ 
                service ^. SAppAppId `in_` valList ids
            orderBy [ desc (version ^. SVersionNumber) ]
            Database.Esqueleto.Experimental.groupBy $ (service ^. SAppAppId, version ^. SVersionNumber)
            pure (service ^. SAppAppId, arrayAggDistinct (version ^. SVersionNumber))
    (categories, versions) <- UnliftIO.Async.concurrently categoriesQuery versionsQuery
    let c = foreach categories $ \(appId, categories') -> (unValue appId, catMaybes $ fromMaybe [] (unValue categories'))
    let v = foreach versions $ \(appId, versions') -> (unValue appId, fromMaybe [] (unValue versions'))
    pure $ HM.intersectionWith (\ vers cts -> (vers, cts)) (HM.fromList v) (HM.fromList c)

-- fetchPackageMetadata :: MonadIO m => [AppIdentifier] -> ReaderT SqlBackend m [PackageMetadata]
fetchPackageMetadata_ :: (MonadLogger m, MonadIO m) => [AppIdentifier] -> AppSettings -> m [PackageMetadata]
fetchPackageMetadata_ ids settings = do
    let connString = PP.pgConnStr $ appDatabaseConf settings
    conn <- liftIO $ PS.connectPostgreSQL connString
    res <- liftIO $ PS.query conn query $ PS.Only $ PS.In ids
    $logInfo $ show query
    $logInfo$ show res
    $logInfo$ show ids
    forM res $ \(appId, versions, categories) ->
        pure $ PackageMetadata 
        { packageMetadataId = appId
        , packageMetadataVersions = versions
        , packageMetadataCategories = categories
        }
    where
        query :: PS.Query
        query = "SELECT s.app_id, json_agg(DISTINCT v.number ORDER BY v.number DESC) AS versions, json_agg(DISTINCT c.category_name) AS categories FROM s_app s LEFT JOIN service_category c on s.id = c.service_id JOIN version v on v.app_id = s.id WHERE s.app_id IN ? GROUP BY s.app_id"
        -- query = "SELECT \"s_app\".\"app_id\", json_agg(DISTINCT \"version\".\"number\" ORDER BY \"version\".\"number\" DESC) AS \"versions\", json_agg(DISTINCT \"service_category\".\"category_name\") AS \"categories\" FROM \"s_app\" LEFT JOIN \"service_category\" on \"s_app\".\"id\" = \"service_category\".\"service_id\" JOIN \"version\" on \"version\".\"app_id\" = \"s_app\".\"id\" WHERE \"s_app\".\"app_id\" IN ? GROUP BY \"s_app\".\"app_id\""

fetchAppCategories :: MonadIO m => Key SApp -> ReaderT SqlBackend m [P.Entity ServiceCategory]
fetchAppCategories appId = select $ do
    (categories :& service) <-
        from
        $           table @ServiceCategory
        `innerJoin` table @SApp
        `on`        (\(sc :& s) -> sc ^. ServiceCategoryServiceId ==. s ^. SAppId)
    where_ (service ^. SAppId ==. val appId)
    pure categories

mapEntityToStoreApp :: MonadIO m => Entity SApp -> ReaderT SqlBackend m StoreApp
mapEntityToStoreApp serviceEntity = do
<<<<<<< HEAD
    let service = entityVal serviceEntity
    entityVersion <- fetchMostRecentAppVersions $ entityKey serviceEntity
    let vers = entityVal <$> entityVersion
    let vv   = mapSVersionToVersionInfo vers
    pure $ StoreApp { storeAppTitle       = sAppTitle service
                    , storeAppDescShort   = sAppDescShort service
                    , storeAppDescLong    = sAppDescLong service
                    , storeAppVersionInfo = NE.fromList vv
                    , storeAppIconType    = sAppIconType service
                    , storeAppTimestamp   = Just (sAppCreatedAt service) -- case on if updatedAt? or always use updated time? was file timestamp
                    }

mapEntityToServiceAvailable :: (MonadIO m, MonadHandler m)
                            => Text
                            -> Entity SApp
                            -> ReaderT SqlBackend m ServiceAvailable
=======
        let service = entityVal serviceEntity
        entityVersion <- fetchMostRecentAppVersions $ entityKey serviceEntity
        let vers = entityVal <$> entityVersion
        let vv = mapSVersionToVersionInfo vers
        pure $ StoreApp {
        storeAppTitle = sAppTitle service
        , storeAppDescShort = sAppDescShort service
        , storeAppDescLong = sAppDescLong service
        , storeAppVersionInfo = NE.fromList vv
        , storeAppIconType = sAppIconType service
        , storeAppTimestamp = Just (sAppCreatedAt service) -- case on if updatedAt? or always use updated time? was file timestamp
        }
        where
            mapSVersionToVersionInfo :: [SVersion] -> [VersionInfo]
            mapSVersionToVersionInfo sv = do
                (\v -> VersionInfo {
                versionInfoVersion = sVersionNumber v
                , versionInfoReleaseNotes = sVersionReleaseNotes v
                , versionInfoDependencies = HM.empty
                , versionInfoOsRequired = sVersionOsVersionRequired v
                , versionInfoOsRecommended = sVersionOsVersionRecommended v
                , versionInfoInstallAlert = Nothing
                }) <$> sv

mapEntityToServiceAvailable :: (MonadIO m, MonadHandler m) => Text -> Entity SApp ->  ReaderT SqlBackend m ServiceAvailable
>>>>>>> aggregate query functions
mapEntityToServiceAvailable domain service = do
    let appId = sAppAppId $ entityVal service
    (_, v) <- fetchLatestApp appId >>= errOnNothing status404 "service not found"
    let appVersion = sVersionNumber (entityVal v)
    pure $ ServiceAvailable { serviceAvailableId        = appId
                            , serviceAvailableTitle     = sAppTitle $ entityVal service
                            , serviceAvailableDescShort = sAppDescShort $ entityVal service
                            , serviceAvailableVersion   = appVersion
                            , serviceAvailableIcon = [i|https://#{domain}/package/icon/#{appId}?spec==#{appVersion}|]
                            }

-- >>> encode hm
-- "{\"0.2.0\":\"some notes\"}"
hm :: Data.Aeson.Value
hm = object [ t .= v | (k, v) <- [("0.2.0", "some notes") :: (Version, Text)], let (String t) = toJSON k ]

-- >>> encode rn
-- "{\"0.2.0\":\"notes one\",\"0.3.0\":\"notes two\"}"
rn :: ReleaseNotes
rn = ReleaseNotes $ HM.fromList [("0.2.0", "notes one"), ("0.3.0", "notes two")]

-- >>> readMaybe $ cc :: Maybe CategoryTitle
-- Just FEATURED
cc :: Text
cc = T.toUpper "featured"

-- >>> encode ccc
-- "\"featured\""
ccc :: CategoryTitle
ccc = FEATURED

