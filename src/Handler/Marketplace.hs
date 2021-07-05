{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TemplateHaskell  #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE QuasiQuotes #-}


module Handler.Marketplace where
import Startlude hiding (from, Handler, on)
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
import Data.HashMap.Strict (HashMap)
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
    { serviceResIcon :: Text
    , serviceResManifest :: ServiceManifest
    , serviceResCategories :: [CategoryTitle]
    , serviceResVersions :: [Version]
    , serviceResDependencyInfo :: HashMap AppIdentifier DependencyInfo
    , serviceResReleaseNotes :: HashMap Version Text
    } deriving (Show)
instance ToJSON ServiceRes where
    toJSON ServiceRes {..} = object
        [ "icon" .= serviceResIcon
        , "manifest" .= serviceResManifest
        , "categories" .= serviceResCategories
        , "versions" .= serviceResVersions
        , "dependency-metadata" .= serviceResDependencyInfo
        , "release-notes" .= serviceResReleaseNotes
        ]
instance ToContent ServiceRes where
    toContent = toContent . toJSON
instance ToTypedContent ServiceRes where
    toTypedContent = toTypedContent . toJSON
data DependencyInfo = DependencyInfo
    { dependencyInfoTitle :: Text -- title
    , dependencyInfoIcon :: Text -- url
    } deriving (Eq, Show)
instance ToJSON DependencyInfo where
    toJSON DependencyInfo {..} = object
        [ "icon" .= dependencyInfoIcon
        , "title" .= dependencyInfoTitle
        ]

data ServiceListRes = ServiceListRes {
    serviceListResCategories :: [CategoryTitle]
    , serviceListResServices :: [ServiceAvailable]
} deriving (Show)
instance ToJSON ServiceListRes where
    toJSON ServiceListRes {..} = object
        [ "categories" .= serviceListResCategories
        , "services" .= serviceListResServices
        ]
instance ToContent ServiceListRes where
    toContent = toContent . toJSON
instance ToTypedContent ServiceListRes where
    toTypedContent = toTypedContent . toJSON

data ServiceAvailable = ServiceAvailable
    { serviceAvailableId :: Text
    , serviceAvailableTitle :: Text
    , serviceAvailableVersion :: Version
    , serviceAvailableIcon :: URL
    , serviceAvailableDescShort :: Text
    } deriving (Show)
instance ToJSON ServiceAvailable where
    toJSON ServiceAvailable { .. } = object
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

data OrderArrangement = ASC | DESC
    deriving (Eq, Show, Read)
data ServiceListDefaults = ServiceListDefaults
    { serviceListOrder :: OrderArrangement
    , serviceListPageLimit :: Int64 -- the number of items per page
    , serviceListPageNumber :: Int64 -- the page you are on
    , serviceListCategory :: CategoryTitle
    , serviceListQuery :: Text
    }
    deriving (Eq, Show, Read)

getCategoriesR :: Handler CategoryRes
getCategoriesR = do
    allCategories <- runDB $ select $ do from $ table @Category
    pure $ CategoryRes $ categoryName . entityVal <$>allCategories

getServiceListR :: Handler ServiceListRes
getServiceListR = do
    getParameters <- reqGetParams <$> getRequest
    let defaults = ServiceListDefaults {
              serviceListOrder = DESC
            , serviceListPageLimit = 20
            , serviceListPageNumber = 1
            , serviceListCategory = ANY
            , serviceListQuery = ""
        }
    category <- case lookup "category" getParameters of
                    Nothing -> pure $ serviceListCategory defaults
                    Just c -> case readMaybe $ T.toUpper c of
                        Nothing -> do
                            $logInfo c
                            sendResponseStatus status400 ("could not read category" :: Text)
                        Just t -> pure t
    page <- case lookup "page" getParameters of
                    Nothing -> pure $ serviceListPageNumber defaults
                    Just p -> case readMaybe p of
                        Nothing -> do
                            $logInfo p
                            sendResponseStatus status400 ("could not read page" :: Text)
                        Just t -> pure $ case t of
                                0 -> 1 -- disallow page 0 so offset is not negative
                                _ -> t
    limit' <- case lookup "per-page" getParameters of
                    Nothing -> pure $ serviceListPageLimit defaults
                    Just c -> case readMaybe $ toS c of
                        Nothing -> sendResponseStatus status400 ("could not read per-page" :: Text)
                        Just l -> pure l
    query <- T.filter (not . isSpace) . fromMaybe (serviceListQuery defaults) <$> lookupGetParam "query"
    $logInfo $ show category
    filteredServices <- runDB $ searchServices category limit' ((page - 1) * limit') query
    domain <- getsYesod $ registryHostname . appSettings
    (appsDir, appMgrDir) <- getsYesod $ ((</> "apps") . resourcesDir &&& staticBinDir) . appSettings
    services <- runDB $ traverse (mapEntityToServiceAvailable appMgrDir appsDir domain) filteredServices
    pure $ ServiceListRes {
          serviceListResCategories = [FEATURED .. MESSAGING]
        , serviceListResServices = services
    }

-- >>> readMaybe $ "0.3.0" :: Maybe Version
-- Just 0.3.0

getServiceR :: Handler ServiceRes
getServiceR = do
    getParameters <- reqGetParams <$> getRequest
    (service, version) <- case lookup "id" getParameters of
                    Nothing -> sendResponseStatus status404 ("id param should exist" :: Text)
                    Just appId' -> do
                        case lookup "version" getParameters of
                            -- default to latest - @TODO need to determine best available based on OS version?
                            Nothing -> runDB $ fetchLatestApp appId' >>= errOnNothing status404 "service not found"
                            Just v -> do
                                case readMaybe v of 
                                    Nothing -> sendResponseStatus status400 ("Invalid App Version Specification" :: Text)
                                    Just vv -> runDB $ fetchLatestAppAtVersion appId' vv >>= errOnNothing status404 ("service at version " <> show v <> " not found")
    (versions, mappedVersions) <- fetchAllAppVersions (entityKey service)
    categories <- runDB $ fetchAppCategories (entityKey service)
    (appsDir, appMgrDir) <- getsYesod $ ((</> "apps") . resourcesDir &&& staticBinDir) . appSettings
    domain <- getsYesod $ registryHostname . appSettings
    let appId = sAppAppId $ entityVal service
    let appDir = (<> "/") . (</> show (sVersionNumber $ entityVal version)) . (</> toS appId) $ appsDir
    let appExt = Extension (toS appId) :: Extension "s9pk"
    manifest' <- handleS9ErrT $ getManifest appMgrDir appDir appExt
    manifest <- case eitherDecode $ BS.fromStrict manifest' of
            Left e -> do
                $logError "could not parse service manifest!"
                $logError (show e)
                sendResponseStatus status500 ("Internal Server Error" :: Text)
            Right (a :: ServiceManifest) -> pure a
    d <- traverse (mapDependencyMetadata appsDir appMgrDir domain) (HM.toList $ serviceManifestDependencies manifest)
    icon <- decodeIcon appMgrDir appsDir appExt
    addPackageHeader appMgrDir appDir appExt
    pure $ ServiceRes
        { serviceResIcon = icon
        , serviceResManifest = manifest -- TypedContent "application/json" (toContent manifest)
        , serviceResCategories = serviceCategoryCategoryName . entityVal <$> categories
        , serviceResVersions = versionInfoVersion <$> versions
        , serviceResDependencyInfo = HM.fromList d
        , serviceResReleaseNotes = mappedVersions
        }

type URL = Text
mapDependencyMetadata :: (MonadIO m, MonadHandler m) => FilePath -> FilePath -> Text -> (AppIdentifier, ServiceDependencyInfo) -> m (AppIdentifier, DependencyInfo)
mapDependencyMetadata appsDir appmgrPath domain (appId, depInfo) = do
    let ext = (Extension (toS appId) :: Extension "s9pk")
    -- get best version from VersionRange of dependency
    version <- getBestVersion appsDir ext (serviceDependencyInfoVersion depInfo) >>= \case
        Nothing -> sendResponseStatus status400 ("Specified App Version Not Found" :: Text)
        Just v -> pure v
    let depPath = appsDir </> toS appId </> show version
    -- @TODO uncomment when sdk icon working
    -- icon <- decodeIcon appmgrPath depPath ext
    pure (appId, DependencyInfo
            { dependencyInfoTitle = appId
            , dependencyInfoIcon = [i|https://#{domain}/icons/#{appId}.png|]

            })

decodeIcon :: (MonadHandler m, KnownSymbol a) => FilePath -> FilePath -> Extension a -> m URL
decodeIcon appmgrPath depPath e@(Extension icon) = do
    icon' <- handleS9ErrT $ getIcon appmgrPath depPath e
    case eitherDecode $ BS.fromStrict icon' of
        Left e' -> do
            $logInfo $ T.pack e'
            sendResponseStatus status400 e'
        Right (i' :: URL) -> pure $ i' <> T.pack icon

fetchAllAppVersions :: Key SApp -> HandlerFor RegistryCtx ([VersionInfo], HashMap Version Text)
fetchAllAppVersions appId = do
    entityAppVersions <- runDB $ P.selectList [SVersionAppId P.==. appId] [] -- orderby version
    let vers = entityVal <$> entityAppVersions
    let vv = mapSVersionToVersionInfo vers
    let mappedVersions = HM.fromList $ (\v -> (versionInfoVersion v, versionInfoReleaseNotes v)) <$> vv
    pure (vv, mappedVersions)

fetchMostRecentAppVersions :: MonadIO m => Key SApp -> ReaderT SqlBackend m [Entity SVersion]
fetchMostRecentAppVersions appId = select $ do
                                        version <- from $ table @SVersion
                                        where_ (version ^. SVersionAppId ==. val appId)
                                        orderBy [ asc (version ^. SVersionNumber) ]
                                        limit 1
                                        pure version

fetchLatestApp :: MonadIO m => Text -> ReaderT SqlBackend m (Maybe (P.Entity SApp, P.Entity SVersion))
fetchLatestApp appId = selectOne $ do
                            (service :& version) <-
                                from $ table @SApp
                                `innerJoin` table @SVersion
                                `on` (\(service :& version) ->
                                        service ^. SAppId ==. version ^. SVersionAppId)
                            where_ (service ^. SAppAppId ==. val appId)
                            orderBy [ desc (version ^. SVersionNumber)]
                            pure (service, version)

fetchLatestAppAtVersion :: MonadIO m => Text -> Version -> ReaderT SqlBackend m (Maybe (P.Entity SApp, P.Entity SVersion))
fetchLatestAppAtVersion appId version' = selectOne $ do
                                                (service :& version) <-
                                                    from $ table @SApp
                                                    `innerJoin` table @SVersion
                                                    `on` (\(service :& version) ->
                                                            service ^. SAppId ==. version ^. SVersionAppId)
                                                where_ $ (service ^. SAppAppId ==. val appId)
                                                    &&. (version ^. SVersionNumber ==. val version')
                                                pure (service, version)

fetchAppCategories :: MonadIO m => Key SApp -> ReaderT SqlBackend m [P.Entity ServiceCategory]
fetchAppCategories appId = select $ do
                                        (categories :& service) <-
                                            from $ table @ServiceCategory
                                            `innerJoin` table @SApp
                                            `on` (\(sc :& s) ->
                                                    sc ^. ServiceCategoryServiceId ==. s ^. SAppId)
                                        where_ (service ^. SAppId ==. val appId)
                                        pure categories

mapEntityToStoreApp :: MonadIO m => Entity SApp -> ReaderT SqlBackend m StoreApp
mapEntityToStoreApp serviceEntity = do
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

mapEntityToServiceAvailable :: (MonadIO m, MonadHandler m) => FilePath -> FilePath -> Text -> Entity SApp ->  ReaderT SqlBackend m ServiceAvailable
mapEntityToServiceAvailable appMgrDir appsDir domain service = do
        -- @TODO uncomment and replace icon when portable embassy-sdk live
        -- icon <- decodeIcon appMgrDir appsDir (Extension "png")
        let appId = sAppAppId $ entityVal service
        let icon = [i|https://#{domain}/icons/#{appId}.png|]
        (_, v) <- fetchLatestApp appId >>= errOnNothing status404 "service not found"
        let appVersion = sVersionNumber (entityVal v)
        pure $ ServiceAvailable
            { serviceAvailableId = appId
            , serviceAvailableTitle = sAppTitle $ entityVal service
            , serviceAvailableDescShort = sAppDescShort $ entityVal service
            , serviceAvailableVersion = appVersion
            , serviceAvailableIcon = icon
            }
