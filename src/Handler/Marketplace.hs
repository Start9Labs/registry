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
import           Conduit                        ( (.|)
                                                , MonadThrow
                                                , mapC
                                                )
import           Data.Aeson
import qualified Data.ByteString.Lazy          as BS
import qualified Data.Conduit.Text             as CT
import qualified Data.HashMap.Strict           as HM
import           Data.List
import qualified Data.List.NonEmpty            as NE
import           Data.Semigroup
import           Data.String.Interpolate.IsString
import qualified Data.Text                     as T
import           Database.Esqueleto.Experimental
import           Database.Esqueleto.PostgreSQL  ( arrayAggDistinct )
import           Database.Marketplace
import qualified Database.Persist              as P
import           Foundation
import           Lib.Error
import           Lib.External.AppMgr
import           Lib.Registry
import           Lib.Types.AppIndex
import           Lib.Types.AppIndex             ( )
import           Lib.Types.Category
import           Lib.Types.Emver
import           Model
import           Network.HTTP.Types
import           Settings
import           Startlude               hiding ( Handler
                                                , from
                                                , on
                                                , sortOn
                                                )
import           System.FilePath.Posix
import           UnliftIO.Async
import           Yesod.Core
import           Yesod.Persist.Core

type URL = Text
newtype CategoryRes = CategoryRes {
    categories :: [CategoryTitle]
} deriving (Show, Generic)
instance ToJSON CategoryRes
instance FromJSON CategoryRes
instance ToContent CategoryRes where
    toContent = toContent . toJSON
instance ToTypedContent CategoryRes where
    toTypedContent = toTypedContent . toJSON
data ServiceRes = ServiceRes
    { serviceResIcon           :: URL
    , serviceResManifest       :: Maybe Data.Aeson.Value -- ServiceManifest
    , serviceResCategories     :: [CategoryTitle]
    , serviceResInstructions   :: URL
    , serviceResLicense        :: URL
    , serviceResVersions       :: [Version]
    , serviceResDependencyInfo :: HM.HashMap AppIdentifier DependencyInfo
    }
    deriving Generic

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
    , dependencyInfoIcon  :: URL
    }
    deriving (Eq, Show)
instance ToJSON DependencyInfo where
    toJSON DependencyInfo {..} = object ["icon" .= dependencyInfoIcon, "title" .= dependencyInfoTitle]

data ServiceListRes = ServiceListRes
    { serviceListResCategories :: [CategoryTitle]
    , serviceListResServices   :: [ServiceAvailable]
    }
    deriving Show
instance ToJSON ServiceListRes where
    toJSON ServiceListRes {..} =
        object ["categories" .= serviceListResCategories, "services" .= serviceListResServices]
instance ToContent ServiceListRes where
    toContent = toContent . toJSON
instance ToTypedContent ServiceListRes where
    toTypedContent = toTypedContent . toJSON

data ServiceAvailable = ServiceAvailable
    { serviceAvailableId        :: AppIdentifier
    , serviceAvailableTitle     :: Text
    , serviceAvailableVersion   :: Version
    , serviceAvailableIcon      :: URL
    , serviceAvailableDescShort :: Text
    }
    deriving Show
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
    { serviceListOrder      :: OrderArrangement
    , serviceListPageLimit  :: Int64 -- the number of items per page
    , serviceListPageNumber :: Int64 -- the page you are on
    , serviceListCategory   :: Maybe CategoryTitle
    , serviceListQuery      :: Text
    }
    deriving (Eq, Show, Read)
data EosRes = EosRes
    { eosResVersion      :: Version
    , eosResHeadline     :: Text
    , eosResReleaseNotes :: ReleaseNotes
    }
    deriving (Eq, Show, Generic)
instance ToJSON EosRes where
    toJSON EosRes {..} =
        object ["version" .= eosResVersion, "headline" .= eosResHeadline, "release-notes" .= eosResReleaseNotes]
instance ToContent EosRes where
    toContent = toContent . toJSON
instance ToTypedContent EosRes where
    toTypedContent = toTypedContent . toJSON

data PackageVersion = PackageVersion
    { packageVersionId      :: AppIdentifier
    , packageVersionVersion :: VersionRange
    }
    deriving Show
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
            (service, _) <- runDB $ fetchLatestApp (AppIdentifier package) >>= errOnNothing status404
                                                                                            "package not found"
            (_, mappedVersions) <- fetchAllAppVersions (entityKey service)
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
                          $   (\v -> (sAppAppId $ entityVal $ fst v, Just $ sVersionNumber $ entityVal $ snd v))
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
            let filteredServices' = sAppAppId . entityVal <$> filteredServices
            settings        <- getsYesod appSettings
            packageMetadata <- runDB $ fetchPackageMetadata
            $logInfo $ show packageMetadata
            serviceDetailResult <- liftIO
                $ mapConcurrently (getServiceDetails settings packageMetadata Nothing) filteredServices'
            let (_, services) = partitionEithers serviceDetailResult
            pure $ ServiceAvailableRes services
            -- if null errors
            --     then pure $ ServiceAvailableRes services
            --     else sendResponseStatus status500 ("Errors acquiring service details: " <> show <$> errors)

        Just packageVersionList -> case eitherDecode $ BS.fromStrict $ encodeUtf8 packageVersionList of
            Left e -> sendResponseStatus status400 ("could not parse query param \"ids\"" <> show e :: Text)
            Right (packages :: [PackageVersion]) -> do
                -- for each item in list get best available from version range
                settings                <- getsYesod appSettings
                -- @TODO fix _ error
                packageMetadata         <- runDB $ fetchPackageMetadata
                availableServicesResult <- traverse (getPackageDetails packageMetadata) packages
                let (_, availableServices) = partitionEithers availableServicesResult
                serviceDetailResult <- liftIO
                    $ mapConcurrently (uncurry $ getServiceDetails settings packageMetadata) availableServices
                -- @TODO fix _ error
                let (_, services) = partitionEithers serviceDetailResult
                pure $ ServiceAvailableRes services
                            -- if null errors
                            --     then pure $ ServiceAvailableRes services
                            --     else sendResponseStatus status500 ("Errors acquiring service details: " <> show <$> errors)































































            where
                getPackageDetails :: MonadIO m
                                  => (HM.HashMap AppIdentifier ([Version], [CategoryTitle]))
                                  -> PackageVersion
                                  -> m (Either Text ((Maybe Version), AppIdentifier))
                getPackageDetails metadata pv = do
                    let appId = packageVersionId pv
                    let spec  = packageVersionVersion pv
                    pacakgeMetadata <- case HM.lookup appId metadata of
                        Nothing -> throwIO $ NotFoundE [i|dependency metadata for #{appId} not found.|]
                        Just m  -> pure m
                    -- get best version from VersionRange of dependency
                    let satisfactory = filter (<|| spec) (fst pacakgeMetadata)
                    let best         = getMax <$> foldMap (Just . Max) satisfactory
                    case best of
                        Nothing ->
                            pure
                                $  Left
                                $  "best version could not be found for "
                                <> show appId
                                <> " with spec "
                                <> show spec
                        Just v -> do
                            pure $ Right (Just v, appId)

getServiceDetails :: (MonadUnliftIO m, Monad m, MonadError IOException m)
                  => AppSettings
                  -> (HM.HashMap AppIdentifier ([Version], [CategoryTitle]))
                  -> Maybe Version
                  -> AppIdentifier
                  -> m (Either Text ServiceRes)
getServiceDetails settings metadata maybeVersion appId = do
    packageMetadata <- case HM.lookup appId metadata of
        Nothing -> throwIO $ NotFoundE [i|#{appId} not found.|]
        Just m  -> pure m
    let (appsDir, appMgrDir) = ((</> "apps") . resourcesDir &&& staticBinDir) settings
    let domain               = registryHostname settings
    version <- case maybeVersion of
        Nothing -> do
            -- grab first value, which will be the latest version
            case fst packageMetadata of
                []    -> throwIO $ NotFoundE $ "no latest version found for " <> show appId
                x : _ -> pure x
        Just v -> pure v
    let appDir = (<> "/") . (</> show version) . (</> show appId) $ appsDir
    let appExt = Extension (show appId) :: Extension "s9pk"
    manifest' <- sourceManifest appMgrDir appDir appExt (\bs -> sinkMem (bs .| mapC BS.fromStrict))
    case eitherDecode $ manifest' of
        Left  e -> pure $ Left $ "Could not parse service manifest for " <> show appId <> ": " <> show e
        Right m -> do
            d <- liftIO $ mapConcurrently (mapDependencyMetadata domain metadata)
                                          (HM.toList $ serviceManifestDependencies m)
            pure $ Right $ ServiceRes { serviceResIcon           = [i|https://#{domain}/package/icon/#{appId}|]
                                      , serviceResManifest       = decode $ manifest' -- pass through raw JSON Value
                                      , serviceResCategories     = snd packageMetadata
                                      , serviceResInstructions   = [i|https://#{domain}/package/instructions/#{appId}|]
                                      , serviceResLicense        = [i|https://#{domain}/package/license/#{appId}|]
                                      , serviceResVersions       = fst packageMetadata
                                      , serviceResDependencyInfo = HM.fromList $ snd $ partitionEithers d
                                      }


mapDependencyMetadata :: (MonadIO m)
                      => Text
                      -> HM.HashMap AppIdentifier ([Version], [CategoryTitle])
                      -> (AppIdentifier, ServiceDependencyInfo)
                      -> m (Either Text (AppIdentifier, DependencyInfo))
mapDependencyMetadata domain metadata (appId, depInfo) = do
    depMetadata <- case HM.lookup appId metadata of
        Nothing -> throwIO $ NotFoundE [i|dependency metadata for #{appId} not found.|]
        Just m  -> pure m
    -- get best version from VersionRange of dependency
    let satisfactory = filter (<|| serviceDependencyInfoVersion depInfo) (fst depMetadata)
    let best         = getMax <$> foldMap (Just . Max) satisfactory
    version <- case best of
        Nothing -> throwIO $ NotFoundE $ "best version not found for dependent package " <> show appId
        Just v  -> pure v
    pure $ Right
        ( appId
        , DependencyInfo { dependencyInfoTitle = appId
                         , dependencyInfoIcon  = [i|https://#{domain}/package/icon/#{appId}?spec==#{version}|]
                         }
        )

-- decodeIcon :: (MonadHandler m, KnownSymbol a) => FilePath -> FilePath -> Extension a -> m URL
-- decodeIcon appmgrPath depPath e@(Extension icon) = do
--     icon' <- handleS9ErrT $ getIcon appmgrPath depPath e
--     case eitherDecode icon' of
--         Left e' -> do
--             $logInfo $ T.pack e'
--             sendResponseStatus status400 e'
--         Right (i' :: URL) -> pure $ i' <> T.pack icon

decodeInstructions :: (MonadUnliftIO m, MonadHandler m, MonadThrow m) => FilePath -> FilePath -> S9PK -> m Text
decodeInstructions appmgrPath depPath package = do
    sourceInstructions appmgrPath depPath package (\bs -> sinkMem (bs .| CT.decode CT.utf8))

decodeLicense :: (MonadUnliftIO m, MonadThrow m, MonadHandler m) => FilePath -> FilePath -> S9PK -> m Text
decodeLicense appmgrPath depPath package =
    sourceLicense appmgrPath depPath package (\bs -> sinkMem (bs .| CT.decode CT.utf8))

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
            (\v -> VersionInfo { versionInfoVersion       = sVersionNumber v
                               , versionInfoReleaseNotes  = sVersionReleaseNotes v
                               , versionInfoDependencies  = HM.empty
                               , versionInfoOsRequired    = sVersionOsVersionRequired v
                               , versionInfoOsRecommended = sVersionOsVersionRecommended v
                               , versionInfoInstallAlert  = Nothing
                               }
                )
                <$> sv

fetchMostRecentAppVersions :: MonadIO m => Key SApp -> ReaderT SqlBackend m [Entity SVersion]
fetchMostRecentAppVersions appId = select $ do
    version <- from $ table @SVersion
    where_ (version ^. SVersionAppId ==. val appId)
    orderBy [desc (version ^. SVersionNumber)]
    limit 1
    pure version

fetchLatestApp :: MonadIO m => AppIdentifier -> ReaderT SqlBackend m (Maybe (P.Entity SApp, P.Entity SVersion))
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
                        => AppIdentifier
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

fetchPackageMetadata :: (MonadLogger m, MonadUnliftIO m)
                     => ReaderT SqlBackend m (HM.HashMap AppIdentifier ([Version], [CategoryTitle]))
fetchPackageMetadata = do
    let categoriesQuery = select $ do
            (service :& category) <-
                from
                $          table @SApp
                `leftJoin` table @ServiceCategory
                `on`       (\(service :& category) ->
                               Database.Esqueleto.Experimental.just (service ^. SAppId)
                                   ==. category
                                   ?.  ServiceCategoryServiceId
                           )
            Database.Esqueleto.Experimental.groupBy $ service ^. SAppAppId
            pure (service ^. SAppAppId, arrayAggDistinct (category ?. ServiceCategoryCategoryName))
    let versionsQuery = select $ do
            (service :& version) <-
                from
                $           table @SApp
                `innerJoin` table @SVersion
                `on`        (\(service :& version) -> (service ^. SAppId) ==. version ^. SVersionAppId)
            orderBy [desc (version ^. SVersionNumber)]
            Database.Esqueleto.Experimental.groupBy $ (service ^. SAppAppId, version ^. SVersionNumber)
            pure (service ^. SAppAppId, arrayAggDistinct (version ^. SVersionNumber))
    (categories, versions) <- UnliftIO.Async.concurrently categoriesQuery versionsQuery
    let
        c = foreach categories
            $ \(appId, categories') -> (unValue appId, catMaybes $ fromMaybe [] (unValue categories'))
    let v = foreach versions $ \(appId, versions') -> (unValue appId, fromMaybe [] (unValue versions'))
    let vv = HM.fromListWithKey (\_ vers vers' -> (++) vers vers') v
    pure $ HM.intersectionWith (\vers cts -> (cts, vers)) (HM.fromList c) vv

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
    where
        mapSVersionToVersionInfo :: [SVersion] -> [VersionInfo]
        mapSVersionToVersionInfo sv = do
            (\v -> VersionInfo { versionInfoVersion       = sVersionNumber v
                               , versionInfoReleaseNotes  = sVersionReleaseNotes v
                               , versionInfoDependencies  = HM.empty
                               , versionInfoOsRequired    = sVersionOsVersionRequired v
                               , versionInfoOsRecommended = sVersionOsVersionRecommended v
                               , versionInfoInstallAlert  = Nothing
                               }
                )
                <$> sv

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

