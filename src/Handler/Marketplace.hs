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

import           Startlude               hiding ( Handler
                                                , from
                                                , on
                                                , sortOn
                                                )

import           Conduit                        ( (.|)
                                                , awaitForever
                                                , runConduit
                                                , sourceFile
                                                )
import           Control.Monad.Except.CoHas     ( liftEither )
import           Control.Parallel.Strategies    ( parMap
                                                , rpar
                                                )
import           Data.Aeson                     ( (.:)
                                                , FromJSON(parseJSON)
                                                , KeyValue((.=))
                                                , ToJSON(toJSON)
                                                , Value(String)
                                                , decode
                                                , eitherDecode
                                                , eitherDecodeStrict
                                                , object
                                                , withObject
                                                )
import qualified Data.Attoparsec.Text          as Atto
import qualified Data.ByteString.Lazy          as BS
import qualified Data.Conduit.List             as CL
import qualified Data.HashMap.Strict           as HM
import           Data.List                      ( head
                                                , lookup
                                                , sortOn
                                                )
import           Data.Semigroup                 ( Max(Max, getMax) )
import           Data.String.Interpolate.IsString
                                                ( i )
import qualified Data.Text                     as T
import           Database.Esqueleto.Experimental
                                                ( (&&.)
                                                , (:&)((:&))
                                                , (==.)
                                                , (?.)
                                                , Entity(entityKey, entityVal)
                                                , PersistEntity(Key)
                                                , SqlBackend
                                                , Value(unValue)
                                                , (^.)
                                                , desc
                                                , from
                                                , groupBy
                                                , innerJoin
                                                , just
                                                , leftJoin
                                                , limit
                                                , on
                                                , orderBy
                                                , select
                                                , selectOne
                                                , table
                                                , val
                                                , where_
                                                )
import           Database.Esqueleto.PostgreSQL  ( arrayAggDistinct )
import           Database.Marketplace           ( searchServices )
import qualified Database.Persist              as P
import           Foundation                     ( Handler
                                                , RegistryCtx(appSettings)
                                                )
import           Lib.Error                      ( S9Error(..) )
import           Lib.PkgRepository              ( getManifest )
import           Lib.Types.AppIndex             ( PkgId(PkgId)
                                                , ServiceDependencyInfo(serviceDependencyInfoVersion)
                                                , ServiceManifest(serviceManifestDependencies)
                                                , VersionInfo(..)
                                                )
import           Lib.Types.AppIndex             ( )
import           Lib.Types.Category             ( CategoryTitle(FEATURED) )
import           Lib.Types.Emver                ( (<||)
                                                , Version
                                                , VersionRange
                                                , parseVersion
                                                , satisfies
                                                )
import           Model                          ( Category(..)
                                                , EntityField(..)
                                                , OsVersion(..)
                                                , SApp(..)
                                                , SVersion(..)
                                                , ServiceCategory
                                                )
import           Network.HTTP.Types             ( status400
                                                , status404
                                                )
import           Protolude.Unsafe               ( unsafeFromJust )
import           Settings                       ( AppSettings(registryHostname, resourcesDir) )
import           System.FilePath                ( (</>) )
import           UnliftIO.Async                 ( concurrently
                                                , mapConcurrently
                                                )
import           UnliftIO.Directory             ( listDirectory )
import           Util.Shared                    ( getVersionSpecFromQuery
                                                , orThrow
                                                )
import           Yesod.Core                     ( HandlerFor
                                                , MonadLogger
                                                , MonadResource
                                                , MonadUnliftIO
                                                , ToContent(..)
                                                , ToTypedContent(..)
                                                , TypedContent
                                                , YesodRequest(..)
                                                , getRequest
                                                , getsYesod
                                                , logWarn
                                                , lookupGetParam
                                                , respondSource
                                                , sendChunkBS
                                                , sendResponseStatus
                                                , typeOctet
                                                )
import           Yesod.Persist.Core             ( YesodPersist(runDB) )

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
    , serviceResManifest       :: Data.Aeson.Value -- ServiceManifest
    , serviceResCategories     :: [CategoryTitle]
    , serviceResInstructions   :: URL
    , serviceResLicense        :: URL
    , serviceResVersions       :: [Version]
    , serviceResDependencyInfo :: HM.HashMap PkgId DependencyInfo
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
    { dependencyInfoTitle :: PkgId
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
    { serviceAvailableId        :: PkgId
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

newtype VersionLatestRes = VersionLatestRes (HM.HashMap PkgId (Maybe Version))
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
    { packageVersionId      :: PkgId
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

getEosVersionR :: Handler EosRes
getEosVersionR = do
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
        Nothing      -> sendResponseStatus status400 (InvalidParamsE "get:id" "<MISSING>")
        Just package -> do
            (service, _) <- runDB $ fetchLatestApp (PkgId package) `orThrow` sendResponseStatus
                status404
                (NotFoundE $ show package)
            (_, mappedVersions) <- fetchAllAppVersions (entityKey service)
            pure mappedVersions

getEosR :: Handler TypedContent
getEosR = do
    spec    <- getVersionSpecFromQuery
    root    <- getsYesod $ (</> "eos") . resourcesDir . appSettings
    subdirs <- listDirectory root
    let (failures, successes) = partitionEithers $ (Atto.parseOnly parseVersion . T.pack) <$> subdirs
    for_ failures $ \f -> $logWarn [i|Emver Parse Failure for EOS: #{f}|]
    let res = headMay . sortOn Down . filter (`satisfies` spec) $ successes
    case res of
        Nothing -> sendResponseStatus status404 (NotFoundE [i|EOS version satisfying #{spec}|])
        Just r  -> do
            let imgPath = root </> show r </> "eos.img"
            respondSource typeOctet (sourceFile imgPath .| awaitForever sendChunkBS)

getVersionLatestR :: Handler VersionLatestRes
getVersionLatestR = do
    getParameters <- reqGetParams <$> getRequest
    case lookup "ids" getParameters of
        Nothing       -> sendResponseStatus status400 (InvalidParamsE "get:ids" "<MISSING>")
        Just packages -> case eitherDecode $ BS.fromStrict $ encodeUtf8 packages of
            Left  _              -> sendResponseStatus status400 (InvalidParamsE "get:ids" packages)
            Right (p :: [PkgId]) -> do
                let packageList :: [(PkgId, Maybe Version)] = (, Nothing) <$> p
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
    pkgIds <- getPkgIdsQuery
    case pkgIds of
        Nothing -> do
            -- query for all
            category         <- getCategoryQuery
            page             <- getPageQuery
            limit'           <- getLimitQuery
            query            <- T.strip . fromMaybe (serviceListQuery defaults) <$> lookupGetParam "query"
            filteredServices <- runDB $ searchServices category limit' ((page - 1) * limit') query
            let filteredServices' = sAppAppId . entityVal <$> filteredServices
            settings            <- getsYesod appSettings
            packageMetadata     <- runDB $ fetchPackageMetadata
            serviceDetailResult <- mapConcurrently (getServiceDetails settings packageMetadata Nothing)
                                                   filteredServices'
            let (_, services) = partitionEithers serviceDetailResult
            pure $ ServiceAvailableRes services

        Just packages -> do
                -- for each item in list get best available from version range
            settings                <- getsYesod appSettings
            -- @TODO fix _ error
            packageMetadata         <- runDB $ fetchPackageMetadata
            availableServicesResult <- traverse (getPackageDetails packageMetadata) packages
            let (_, availableServices) = partitionEithers availableServicesResult
            serviceDetailResult <- mapConcurrently (uncurry $ getServiceDetails settings packageMetadata)
                                                   availableServices
            -- @TODO fix _ error
            let (_, services) = partitionEithers serviceDetailResult
            pure $ ServiceAvailableRes services
    where
        defaults = ServiceListDefaults { serviceListOrder      = DESC
                                       , serviceListPageLimit  = 20
                                       , serviceListPageNumber = 1
                                       , serviceListCategory   = Nothing
                                       , serviceListQuery      = ""
                                       }
        getPkgIdsQuery :: Handler (Maybe [PackageVersion])
        getPkgIdsQuery = lookupGetParam "ids" >>= \case
            Nothing  -> pure Nothing
            Just ids -> case eitherDecodeStrict (encodeUtf8 ids) of
                Left _ -> do
                    let e = InvalidParamsE "get:ids" ids
                    $logWarn (show e)
                    sendResponseStatus status400 e
                Right a -> pure a
        getCategoryQuery :: Handler (Maybe CategoryTitle)
        getCategoryQuery = lookupGetParam "category" >>= \case
            Nothing -> pure Nothing
            Just c  -> case readMaybe . T.toUpper $ c of
                Nothing -> do
                    let e = InvalidParamsE "get:category" c
                    $logWarn (show e)
                    sendResponseStatus status400 e
                Just t -> pure $ Just t
        getPageQuery :: Handler Int64
        getPageQuery = lookupGetParam "page" >>= \case
            Nothing -> pure $ serviceListPageNumber defaults
            Just p  -> case readMaybe p of
                Nothing -> do
                    let e = InvalidParamsE "get:page" p
                    $logWarn (show e)
                    sendResponseStatus status400 e
                Just t -> pure $ case t of
                    0 -> 1 -- disallow page 0 so offset is not negative
                    _ -> t
        getLimitQuery :: Handler Int64
        getLimitQuery = lookupGetParam "per-page" >>= \case
            Nothing -> pure $ serviceListPageLimit defaults
            Just pp -> case readMaybe pp of
                Nothing -> do
                    let e = InvalidParamsE "get:per-page" pp
                    $logWarn (show e)
                    sendResponseStatus status400 e
                Just l -> pure l
        getPackageDetails :: MonadIO m
                          => (HM.HashMap PkgId ([Version], [CategoryTitle]))
                          -> PackageVersion
                          -> m (Either Text ((Maybe Version), PkgId))
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
                Nothing -> pure $ Left $ [i|Best version could not be found for #{appId} with spec #{spec}|]
                Just v  -> do
                    pure $ Right (Just v, appId)

getServiceDetails :: (MonadIO m, MonadResource m)
                  => AppSettings
                  -> (HM.HashMap PkgId ([Version], [CategoryTitle]))
                  -> Maybe Version
                  -> PkgId
                  -> m (Either S9Error ServiceRes)
getServiceDetails settings metadata maybeVersion pkg = runExceptT $ do
    packageMetadata <- case HM.lookup pkg metadata of
        Nothing -> liftEither . Left $ NotFoundE [i|#{pkg} not found.|]
        Just m  -> pure m
    let domain = registryHostname settings
    version <- case maybeVersion of
        Nothing -> do
            -- grab first value, which will be the latest version
            case fst packageMetadata of
                []    -> liftEither . Left $ NotFoundE $ [i|No latest version found for #{pkg}|]
                x : _ -> pure x
        Just v -> pure v
    manifest <- flip runReaderT settings $ (snd <$> getManifest pkg version) >>= \bs ->
        runConduit $ bs .| CL.foldMap BS.fromStrict
    case eitherDecode manifest of
        Left  _ -> liftEither . Left $ AssetParseE [i|#{pkg}:manifest|] (decodeUtf8 $ BS.toStrict manifest)
        Right m -> do
            let d = parMap rpar (mapDependencyMetadata domain metadata) (HM.toList $ serviceManifestDependencies m)
            pure $ ServiceRes { serviceResIcon           = [i|https://#{domain}/package/icon/#{pkg}|]
                                        -- pass through raw JSON Value, we have checked its correct parsing above
                              , serviceResManifest       = unsafeFromJust . decode $ manifest
                              , serviceResCategories     = snd packageMetadata
                              , serviceResInstructions   = [i|https://#{domain}/package/instructions/#{pkg}|]
                              , serviceResLicense        = [i|https://#{domain}/package/license/#{pkg}|]
                              , serviceResVersions       = fst packageMetadata
                              , serviceResDependencyInfo = HM.fromList $ snd $ partitionEithers d
                              }

mapDependencyMetadata :: Text
                      -> HM.HashMap PkgId ([Version], [CategoryTitle])
                      -> (PkgId, ServiceDependencyInfo)
                      -> Either S9Error (PkgId, DependencyInfo)
mapDependencyMetadata domain metadata (appId, depInfo) = do
    depMetadata <- case HM.lookup appId metadata of
        Nothing -> Left $ NotFoundE [i|dependency metadata for #{appId} not found.|]
        Just m  -> pure m
    -- get best version from VersionRange of dependency
    let satisfactory = filter (<|| serviceDependencyInfoVersion depInfo) (fst depMetadata)
    let best         = getMax <$> foldMap (Just . Max) satisfactory
    version <- case best of
        Nothing -> Left $ NotFoundE $ [i|No satisfactory version for dependent package #{appId}|]
        Just v  -> pure v
    pure
        ( appId
        , DependencyInfo { dependencyInfoTitle = appId
                         , dependencyInfoIcon  = [i|https://#{domain}/package/icon/#{appId}?spec==#{version}|]
                         }
        )

fetchAllAppVersions :: Key SApp -> HandlerFor RegistryCtx ([VersionInfo], ReleaseNotes)
fetchAllAppVersions appId = do
    entityAppVersions <- runDB $ P.selectList [SVersionAppId P.==. appId] []
    let vers           = entityVal <$> entityAppVersions
    let vv             = mapSVersionToVersionInfo vers
    let mappedVersions = ReleaseNotes $ HM.fromList $ (\v -> (versionInfoVersion v, versionInfoReleaseNotes v)) <$> vv
    pure (sortOn (Down . versionInfoVersion) vv, mappedVersions)
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
fetchMostRecentAppVersions appId = sortResults $ select $ do
    version <- from $ table @SVersion
    where_ (version ^. SVersionAppId ==. val appId)
    limit 1
    pure version
    where sortResults = fmap $ sortOn (Down . sVersionNumber . entityVal)

fetchLatestApp :: MonadIO m => PkgId -> ReaderT SqlBackend m (Maybe (P.Entity SApp, P.Entity SVersion))
fetchLatestApp appId = fmap headMay . sortResults . select $ do
    (service :& version) <-
        from
        $           table @SApp
        `innerJoin` table @SVersion
        `on`        (\(service :& version) -> service ^. SAppId ==. version ^. SVersionAppId)
    where_ (service ^. SAppAppId ==. val appId)
    pure (service, version)
    where sortResults = fmap $ sortOn (Down . sVersionNumber . entityVal . snd)

fetchLatestAppAtVersion :: MonadIO m
                        => PkgId
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
                     => ReaderT SqlBackend m (HM.HashMap PkgId ([Version], [CategoryTitle]))
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
            Database.Esqueleto.Experimental.groupBy $ (service ^. SAppAppId, version ^. SVersionNumber)
            pure (service ^. SAppAppId, arrayAggDistinct (version ^. SVersionNumber))
    (categories, versions) <- UnliftIO.Async.concurrently categoriesQuery versionsQuery
    let
        c = foreach categories
            $ \(appId, categories') -> (unValue appId, catMaybes $ fromMaybe [] (unValue categories'))
    let v = foreach versions $ \(appId, versions') -> (unValue appId, fromMaybe [] (unValue versions'))
    let vv = HM.fromListWithKey (\_ vers vers' -> (++) vers vers') v
    pure $ HM.intersectionWith (\cts vers -> (vers, cts)) (HM.fromList c) (sortVersions vv)
    where sortVersions = fmap $ sortOn Down

fetchAppCategories :: MonadIO m => Key SApp -> ReaderT SqlBackend m [P.Entity ServiceCategory]
fetchAppCategories appId = select $ do
    (categories :& service) <-
        from
        $           table @ServiceCategory
        `innerJoin` table @SApp
        `on`        (\(sc :& s) -> sc ^. ServiceCategoryServiceId ==. s ^. SAppId)
    where_ (service ^. SAppId ==. val appId)
    pure categories

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

