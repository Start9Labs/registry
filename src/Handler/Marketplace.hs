{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TemplateHaskell  #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleContexts #-}

module Handler.Marketplace where

import           Startlude               hiding ( Any
                                                , Handler
                                                , ask
                                                , concurrently
                                                , from
                                                , on
                                                , sortOn
                                                )

import           Conduit                        ( (.|)
                                                , awaitForever
                                                , dropC
                                                , mapC
                                                , runConduit
                                                , sinkList
                                                , sourceFile
                                                , takeC
                                                , MonadUnliftIO
                                                )
import           Control.Monad.Except.CoHas     ( liftEither )

import           Control.Parallel.Strategies    ( parMap
                                                , rpar
                                                )
import           Crypto.Hash                    ( SHA256 )
import           Crypto.Hash.Conduit            ( hashFile )
import           Data.Aeson                     ( (.:)
                                                , FromJSON(parseJSON)
                                                , KeyValue((.=))
                                                , ToJSON(toJSON)
                                                , Value(String)
                                                , decode
                                                , eitherDecode
                                                , eitherDecodeStrict
                                                )
import qualified Data.Attoparsec.Text          as Atto
import           Data.ByteArray.Encoding        ( Base(Base16)
                                                , convertToBase
                                                )
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
                                                ( Entity(entityKey, entityVal)
                                                , SqlBackend
                                                , (^.)
                                                , desc
                                                , from
                                                , orderBy
                                                , select
                                                , table
                                                )
import           Database.Marketplace           ( filterOsCompatible
                                                , getPkgData
                                                , searchServices
                                                , zipVersions
                                                , fetchAllAppVersions
                                                , fetchLatestApp
                                                , fetchAppCategories
                                                )
import qualified Database.Persist              as P
import           Database.Persist               ( PersistUniqueRead(getBy)
                                                , insertUnique
                                                )
import           Foundation                     ( Handler
                                                , RegistryCtx(appSettings, appConnPool)
                                                )
import           Lib.Error                      ( S9Error(..)
                                                , toStatus
                                                )
import           Lib.PkgRepository              ( getManifest )
import           Lib.Types.AppIndex             ( PkgId(PkgId)
                                                , PackageDependency(packageDependencyVersion)
                                                , PackageManifest(packageManifestDependencies)
                                                )
import           Lib.Types.AppIndex             ( )
import           Lib.Types.Category             ( CategoryTitle(..) )
import           Lib.Types.Emver                ( (<||)
                                                , Version
                                                , VersionRange(Any)
                                                , parseRange
                                                , parseVersion
                                                , satisfies
                                                )
import           Model                          ( Category(..)
                                                , EntityField(..)
                                                , EosHash(EosHash, eosHashHash)
                                                , Key(PkgRecordKey, unPkgRecordKey)
                                                , OsVersion(..)
                                                , PkgRecord(..)
                                                , Unique(UniqueVersion)
                                                , VersionRecord(..)
                                                )
import           Network.HTTP.Types             ( status400
                                                , status404
                                                )
import           Protolude.Unsafe               ( unsafeFromJust )
import           Settings                       ( AppSettings(registryHostname, resourcesDir) )
import           System.Directory               ( getFileSize )
import           System.FilePath                ( (</>) )
import           UnliftIO.Async                 ( concurrently
                                                , mapConcurrently
                                                )
import           UnliftIO.Directory             ( listDirectory )
import           Util.Shared                    ( getVersionSpecFromQuery )
import           Yesod.Core                     ( MonadResource
                                                , TypedContent
                                                , YesodRequest(..)
                                                , addHeader
                                                , getRequest
                                                , getsYesod
                                                , lookupGetParam
                                                , respondSource
                                                , sendChunkBS
                                                , sendResponseStatus
                                                , typeOctet
                                                , getYesod
                                                )
import           Yesod.Persist                  ( YesodDB )
import           Yesod.Persist.Core             ( YesodPersist(runDB) )
import           Data.Tuple.Extra        hiding ( second
                                                , first
                                                , (&&&)
                                                )
import           Control.Monad.Logger
import           Database.Persist.Sql           ( runSqlPool )
import           Database.Persist.Postgresql    ( ConnectionPool )
import           Control.Monad.Reader.Has       ( Has
                                                , ask
                                                )
import           Handler.Types.Marketplace

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
            appConnPool <- appConnPool <$> getYesod
            (_, notes)  <- runDB $ fetchAllAppVersions appConnPool (PkgId package)
            pure notes

getEosR :: Handler TypedContent
getEosR = do
    spec    <- getVersionSpecFromQuery
    root    <- getsYesod $ (</> "eos") . resourcesDir . appSettings
    subdirs <- listDirectory root
    let (failures, successes) = partitionEithers $ (Atto.parseOnly parseVersion . T.pack) <$> subdirs
    for_ failures $ \f -> $logWarn [i|Emver Parse Failure for EOS: #{f}|]
    let mVersion = headMay . sortOn Down . filter (`satisfies` spec) $ successes
    case mVersion of
        Nothing      -> sendResponseStatus status404 (NotFoundE [i|EOS version satisfying #{spec}|])
        Just version -> do
            let imgPath = root </> show version </> "eos.img"
            (sz, h) <- runDB $ concurrently (liftIO $ getFileSize imgPath) (retrieveHash version imgPath)
            addHeader "Content-Length" $ show sz
            addHeader "x-eos-hash" h
            respondSource typeOctet (sourceFile imgPath .| awaitForever sendChunkBS)
    where
        retrieveHash :: Version -> FilePath -> YesodDB RegistryCtx Text
        retrieveHash v fp = do
            mHash <- getBy (UniqueVersion v)
            case mHash of
                Just h  -> pure . eosHashHash . entityVal $ h
                Nothing -> do
                    h <- hashFile @_ @SHA256 fp
                    let t = decodeUtf8 $ convertToBase Base16 h
                    void $ insertUnique (EosHash v t) -- lazily populate
                    pure t

getVersionLatestR :: Handler VersionLatestRes
getVersionLatestR = do
    getParameters <- reqGetParams <$> getRequest
    case lookup "ids" getParameters of
        Nothing       -> sendResponseStatus status400 (InvalidParamsE "get:ids" "<MISSING>")
        Just packages -> case eitherDecode $ BS.fromStrict $ encodeUtf8 packages of
            Left  _ -> sendResponseStatus status400 (InvalidParamsE "get:ids" packages)
            Right p -> do
                let packageList = (, Nothing) <$> p
                found <- runDB $ traverse fetchLatestApp $ fst <$> packageList
                pure
                    $ VersionLatestRes
                    $ HM.union
                          (   HM.fromList
                          $   (\v ->
                                  (unPkgRecordKey . entityKey $ fst v, Just $ versionRecordNumber $ entityVal $ snd v)
                              )
                          <$> catMaybes found
                          )
                    $ HM.fromList packageList

getPackageListR :: Handler PackageListRes
getPackageListR = do
    osPredicate <- getOsVersionQuery <&> \case
        Nothing -> const True
        Just v  -> flip satisfies v
    pkgIds           <- getPkgIdsQuery
    -- deep info
    -- generate data from db
    -- filter os
    -- filter from request
    -- shallow info - generate get deps
    -- transformations
    -- assemble api response
    filteredPackages <- case pkgIds of
        Nothing -> do
            -- query for all
            category <- getCategoryQuery
            page     <- getPageQuery
            limit'   <- getLimitQuery
            query    <- T.strip . fromMaybe (packageListQuery defaults) <$> lookupGetParam "query"
            runDB
                $  runConduit
                $  searchServices category query
                .| zipVersions
                .| mapC (\(a, vs) -> (,,) a vs Any)
                .| filterOsCompatible osPredicate
                -- pages start at 1 for some reason. TODO: make pages start at 0
                .| (dropC (limit' * (page - 1)) *> takeC limit')
                .| sinkList
        Just packages' -> do
            -- for each item in list get best available from version range
            let vMap = (packageReqId &&& packageReqVersion) <$> packages'
            runDB
                .  runConduit
                $  getPkgData (packageReqId <$> packages')
                .| zipVersions
                .| mapC
                       (\(a, vs) ->
                           let spec = fromMaybe Any $ lookup (unPkgRecordKey $ entityKey a) vMap
                           in  (a, filter ((<|| spec) . versionRecordNumber . entityVal) vs, spec)
                       )
                .| filterOsCompatible osPredicate
                .| sinkList
    (keys, packageMetadata) <- runDB $ createPackageMetadata filteredPackages
    appConnPool             <- appConnPool <$> getYesod
    serviceDetailResult     <- mapConcurrently (getServiceDetails osPredicate appConnPool packageMetadata) keys
    let (errors, res) = partitionEithers serviceDetailResult
    case errors of
        x : xs -> do
            -- log all errors but just throw first error until Validation implemented - TODO https://hackage.haskell.org/package/validation
            for_ xs (\e -> $logWarn [i|Get package list errors: #{e}|])
            sendResponseStatus (toStatus x) x
        [] -> pure $ PackageListRes res

    where
        defaults = PackageListDefaults { packageListOrder      = DESC
                                       , packageListPageLimit  = 20
                                       , packageListPageNumber = 1
                                       , packageListCategory   = Nothing
                                       , packageListQuery      = ""
                                       }
        getPkgIdsQuery :: Handler (Maybe [PackageReq])
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
        getPageQuery :: Handler Int
        getPageQuery = lookupGetParam "page" >>= \case
            Nothing -> pure $ packageListPageNumber defaults
            Just p  -> case readMaybe p of
                Nothing -> do
                    let e = InvalidParamsE "get:page" p
                    $logWarn (show e)
                    sendResponseStatus status400 e
                Just t -> pure $ case t of
                    0 -> 1 -- disallow page 0 so offset is not negative
                    _ -> t
        getLimitQuery :: Handler Int
        getLimitQuery = lookupGetParam "per-page" >>= \case
            Nothing -> pure $ packageListPageLimit defaults
            Just pp -> case readMaybe pp of
                Nothing -> do
                    let e = InvalidParamsE "get:per-page" pp
                    $logWarn (show e)
                    sendResponseStatus status400 e
                Just l -> pure l
        getOsVersionQuery :: Handler (Maybe VersionRange)
        getOsVersionQuery = lookupGetParam "eos-version-compat" >>= \case
            Nothing  -> pure Nothing
            Just osv -> case Atto.parseOnly parseRange osv of
                Left _ -> do
                    let e = InvalidParamsE "get:eos-version-compat" osv
                    $logWarn (show e)
                    sendResponseStatus status400 e
                Right v -> pure $ Just v

mergeDupes :: ([Version], VersionRange) -> ([Version], VersionRange) -> ([Version], VersionRange)
mergeDupes (vs, vr) (vs', _) = (,) ((++) vs vs') vr

createPackageMetadata :: (MonadReader r m, MonadIO m)
                      => [(Entity PkgRecord, [Entity VersionRecord], VersionRange)]
                      -> ReaderT
                             SqlBackend
                             m
                             ([PkgId], HM.HashMap PkgId (([Version], VersionRange), [CategoryTitle]))
createPackageMetadata pkgs = do
    let keys = unPkgRecordKey . entityKey . fst3 <$> pkgs
    cats <- fetchAppCategories keys
    let vers =
            pkgs
                <&> first3 (unPkgRecordKey . entityKey)
                <&> second3 (sortOn Down . fmap (versionRecordNumber . entityVal))
                <&> (\(a, vs, vr) -> (,) a $ (,) vs vr)
                &   HM.fromListWith mergeDupes
    pure $ (keys, HM.intersectionWith (,) vers (categoryName <<$>> cats))

getServiceDetails :: (MonadResource m, MonadReader r m, MonadLogger m, Has AppSettings r, MonadUnliftIO m)
                  => (Version -> Bool)
                  -> ConnectionPool
                  -> (HM.HashMap PkgId (([Version], VersionRange), [CategoryTitle]))
                  -> PkgId
                  -> m (Either S9Error PackageRes)
getServiceDetails osPredicate appConnPool metadata pkg = runExceptT $ do
    settings        <- ask
    packageMetadata <- case HM.lookup pkg metadata of
        Nothing -> liftEither . Left $ NotFoundE [i|#{pkg} not found.|]
        Just m  -> pure m
    let domain      = registryHostname settings
    let versionInfo = fst $ (HM.!) metadata pkg
    version <- case snd versionInfo of
        Any -> do
            -- grab first value, which will be the latest version
            case fst versionInfo of
                []    -> liftEither . Left $ NotFoundE $ [i|No latest version found for #{pkg}|]
                x : _ -> pure x
        spec -> case headMay . sortOn Down $ filter (`satisfies` spec) $ fst versionInfo of
            Nothing -> liftEither . Left $ NotFoundE [i|No version for #{pkg} satisfying #{spec}|]
            Just v  -> pure v
    manifest <- flip runReaderT settings $ (snd <$> getManifest pkg version) >>= \bs ->
        runConduit $ bs .| CL.foldMap BS.fromStrict
    case eitherDecode manifest of
        Left  _ -> liftEither . Left $ AssetParseE [i|#{pkg}:manifest|] (decodeUtf8 $ BS.toStrict manifest)
        Right m -> do
            let depVerList = (fst &&& (packageDependencyVersion . snd)) <$> (HM.toList $ packageManifestDependencies m)
            (_, depMetadata) <- lift $ runSqlPool (createPackageMetadata =<< getDependencies depVerList) appConnPool
            let (errors, deps) = partitionEithers $ parMap
                    rpar
                    (mapDependencyMetadata domain $ (HM.union depMetadata metadata))
                    (HM.toList $ packageManifestDependencies m)
            case errors of
                _ : xs -> liftEither . Left $ DepMetadataE xs
                [] -> pure $ PackageRes { packageResIcon         = [i|https://#{domain}/package/icon/#{pkg}|]
                                        -- pass through raw JSON Value, we have checked its correct parsing above
                                        , packageResManifest     = unsafeFromJust . decode $ manifest
                                        , packageResCategories   = snd packageMetadata
                                        , packageResInstructions = [i|https://#{domain}/package/instructions/#{pkg}|]
                                        , packageResLicense      = [i|https://#{domain}/package/license/#{pkg}|]
                                        , packageResVersions     = fst . fst $ packageMetadata
                                        , packageResDependencies = HM.fromList deps
                                        }
    where
        getDependencies :: (MonadResource m, MonadUnliftIO m)
                        => [(PkgId, VersionRange)]
                        -> ReaderT SqlBackend m [(Entity PkgRecord, [Entity VersionRecord], VersionRange)]
        getDependencies deps =
            runConduit
                $  getPkgData (fst <$> deps)
                .| zipVersions
                .| mapC
                       (\(a, vs) ->
                           let spec = fromMaybe Any $ lookup (unPkgRecordKey $ entityKey a) deps
                           in  (a, filter ((<|| spec) . versionRecordNumber . entityVal) vs, spec)
                       )
                .| filterOsCompatible osPredicate
                .| sinkList

mapDependencyMetadata :: Text
                      -> HM.HashMap PkgId (([Version], VersionRange), [CategoryTitle])
                      -> (PkgId, PackageDependency)
                      -> Either Text (PkgId, DependencyRes)
mapDependencyMetadata domain metadata (appId, depInfo) = do
    depMetadata <- case HM.lookup appId metadata of
        Nothing -> Left [i|dependency metadata for #{appId} not found.|]
        Just m  -> pure m
    -- get best version from VersionRange of dependency
    let satisfactory = filter (<|| packageDependencyVersion depInfo) (fst . fst $ depMetadata)
    let best         = getMax <$> foldMap (Just . Max) satisfactory
    version <- case best of
        Nothing -> Left [i|No satisfactory version for dependent package #{appId}|]
        Just v  -> pure v
    pure
        ( appId
        , DependencyRes { dependencyResTitle = appId
                        , dependencyResIcon  = [i|https://#{domain}/package/icon/#{appId}?spec==#{version}|]
                        }
        )
