{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TemplateHaskell  #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TupleSections #-}


module Handler.Marketplace where
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
    { dependencyInfoTitle :: Text -- title
    , dependencyInfoIcon :: Text -- url
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
    { serviceAvailableId :: Text
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
    toJSON EosRes {..} =
        object ["version" .= eosResVersion, "headline" .= eosResHeadline, "release-notes" .= eosResReleaseNotes]
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
            (service, _             ) <- runDB $ fetchLatestApp package >>= errOnNothing status404 "package not found"
            (_      , mappedVersions) <- fetchAllAppVersions (entityKey service)
            pure mappedVersions

getVersionLatestR :: Handler VersionLatestRes
getVersionLatestR = do
    getParameters <- reqGetParams <$> getRequest
    case lookup "ids" getParameters of
        Nothing       -> sendResponseStatus status400 ("expected query param \"ids\" to exist" :: Text)
        Just packages -> case eitherDecode $ BS.fromStrict $ encodeUtf8 packages of
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

getPackageListR :: Handler ServiceAvailableRes
getPackageListR = do
    getParameters <- reqGetParams <$> getRequest
    let defaults = ServiceListDefaults { serviceListOrder      = DESC
                                       , serviceListPageLimit  = 20
                                       , serviceListPageNumber = 1
                                       , serviceListCategory   = Nothing
                                       , serviceListQuery      = ""
                                       }
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
            -- domain <- getsYesod $ registryHostname . appSettings
            -- (appsDir, appMgrDir) <- getsYesod $ ((</> "apps") . resourcesDir &&& staticBinDir) . appSettings
            -- res <- runDB $ traverse (mapEntityToServiceAvailable appMgrDir appsDir domain) filteredServices
            res <- traverse (getServiceDetails Nothing) filteredServices
            pure $ ServiceAvailableRes res

        Just packageVersionList -> case eitherDecode $ BS.fromStrict $ encodeUtf8 packageVersionList of
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
mapDependencyMetadata appsDir domain (appId, depInfo) = do
    let ext = (Extension (toS appId) :: Extension "s9pk")
    -- get best version from VersionRange of dependency
    version <- getBestVersion appsDir ext (serviceDependencyInfoVersion depInfo) >>= \case
        Nothing -> sendResponseStatus status404 ("best version not found for dependent package " <> appId :: Text)
        Just v  -> pure v
    pure
        ( appId
        , DependencyInfo { dependencyInfoTitle = appId
                         , dependencyInfoIcon  = [i|https://#{domain}/package/icon/#{appId}?spec==#{version}|]
                         }
        )

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

fetchMostRecentAppVersions :: MonadIO m => Key SApp -> ReaderT SqlBackend m [Entity SVersion]
fetchMostRecentAppVersions appId = select $ do
    version <- from $ table @SVersion
    where_ (version ^. SVersionAppId ==. val appId)
    orderBy [desc (version ^. SVersionNumber)]
    limit 1
    pure version

fetchLatestApp :: MonadIO m => Text -> ReaderT SqlBackend m (Maybe (P.Entity SApp, P.Entity SVersion))
fetchLatestApp appId = selectOne $ do
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
fetchLatestAppAtVersion appId version' = selectOne $ do
    (service :& version) <-
        from
        $           table @SApp
        `innerJoin` table @SVersion
        `on`        (\(service :& version) -> service ^. SAppId ==. version ^. SVersionAppId)
    where_ $ (service ^. SAppAppId ==. val appId) &&. (version ^. SVersionNumber ==. val version')
    pure (service, version)

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

