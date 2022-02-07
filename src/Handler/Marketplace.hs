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
                                                , runConduit
                                                , sinkList
                                                , sourceFile
                                                , takeC
                                                )
import           Control.Monad.Logger           ( MonadLogger
                                                , logWarn
                                                )
import           Control.Monad.Reader.Has       ( Has
                                                , ask
                                                )
import           Crypto.Hash                    ( SHA256 )
import           Crypto.Hash.Conduit            ( hashFile )
import           Data.Aeson                     ( decode
                                                , eitherDecode
                                                , eitherDecodeStrict
                                                )
import qualified Data.Attoparsec.Text          as Atto

import           Data.ByteArray.Encoding        ( Base(..)
                                                , convertToBase
                                                )
import           Data.ByteString.Base64
import qualified Data.ByteString.Lazy          as LBS
import qualified Data.Conduit.List             as CL
import qualified Data.HashMap.Strict           as HM
import           Data.List                      ( head
                                                , lookup
                                                , sortOn
                                                )
import           Data.String.Interpolate.IsString
                                                ( i )
import qualified Data.Text                     as T
import qualified Data.Text.Lazy                as TL
import qualified Data.Text.Lazy.Builder        as TB
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
import           Database.Marketplace           ( fetchAllAppVersions
                                                , fetchLatestApp
                                                , getPkgData
                                                , getPkgDependencyData
                                                , searchServices
                                                , zipCategories
                                                , zipDependencyVersions
                                                , zipVersions
                                                )
import           Database.Persist               ( PersistUniqueRead(getBy)
                                                , insertUnique
                                                )
import           Foundation                     ( Handler
                                                , RegistryCtx(appConnPool, appSettings)
                                                , Route(InstructionsR, LicenseR)
                                                )
import           Handler.Types.Marketplace
import           Lib.Error                      ( S9Error(..) )
import           Lib.PkgRepository              ( PkgRepo
                                                , getIcon
                                                , getManifest
                                                )
import           Lib.Types.AppIndex             ( PkgId(PkgId) )
import           Lib.Types.AppIndex             ( )
import           Lib.Types.Category             ( CategoryTitle(..) )
import           Lib.Types.Emver                ( Version
                                                , VersionRange
                                                , parseRange
                                                , parseVersion
                                                , satisfies
                                                )
import           Model                          ( Category(..)
                                                , EntityField(..)
                                                , EosHash(EosHash, eosHashHash)
                                                , Key(unPkgRecordKey)
                                                , OsVersion(..)
                                                , PkgRecord(..)
                                                , Unique(UniqueVersion)
                                                , VersionRecord(..)
                                                )
import           Network.HTTP.Types             ( status400
                                                , status404
                                                )
import           Protolude.Unsafe               ( unsafeFromJust )
import           Settings
import           System.Directory               ( getFileSize )
import           System.FilePath                ( (</>) )
import           UnliftIO.Async                 ( concurrently
                                                , mapConcurrently
                                                )
import           UnliftIO.Directory             ( listDirectory )
import           Util.Shared                    ( filterDependencyBestVersion
                                                , filterDependencyOsCompatible
                                                , filterLatestVersionFromSpec
                                                , filterPkgOsCompatible
                                                , getVersionSpecFromQuery
                                                )
import           Yesod.Core                     ( MonadResource
                                                , RenderRoute(renderRoute)
                                                , TypedContent
                                                , YesodRequest(..)
                                                , addHeader
                                                , getRequest
                                                , getYesod
                                                , getsYesod
                                                , lookupGetParam
                                                , respondSource
                                                , sendChunkBS
                                                , sendResponseStatus
                                                , typeOctet
                                                )
import           Yesod.Persist                  ( YesodDB )
import           Yesod.Persist.Core             ( YesodPersist(runDB) )

getInfoR :: Handler InfoRes
getInfoR = do
    name          <- getsYesod $ marketplaceName . appSettings
    allCategories <- runDB $ select $ do
        cats <- from $ table @Category
        orderBy [desc (cats ^. CategoryPriority)]
        pure cats
    pure $ InfoRes name $ categoryName . entityVal <$> allCategories

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
            appConnPool    <- appConnPool <$> getYesod
            versionRecords <- runDB $ fetchAllAppVersions appConnPool (PkgId package)
            pure $ constructReleaseNotesApiRes versionRecords
    where
        constructReleaseNotesApiRes :: [VersionRecord] -> ReleaseNotes
        constructReleaseNotesApiRes vers = do
            ReleaseNotes $ HM.fromList $ sortOn Down $ (versionRecordNumber &&& versionRecordReleaseNotes) <$> vers

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

-- TODO refactor with conduit
getVersionLatestR :: Handler VersionLatestRes
getVersionLatestR = do
    getParameters <- reqGetParams <$> getRequest
    case lookup "ids" getParameters of
        Nothing       -> sendResponseStatus status400 (InvalidParamsE "get:ids" "<MISSING>")
        Just packages -> case eitherDecode $ LBS.fromStrict $ encodeUtf8 packages of
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
                .  runConduit
                $  getPkgData (packageReqId <$> packages')
                .| zipVersions
                .| zipCategories
                .| filterLatestVersionFromSpec vMap
                .| filterPkgOsCompatible osPredicate
                .| sinkList
    -- NOTE: if a package's dependencies do not meet the system requirements, it is currently omitted from the list
    pkgsWithDependencies <- runDB $ mapConcurrently (getPackageDependencies osPredicate) filteredPackages
    PackageListRes <$> mapConcurrently constructPackageListApiRes pkgsWithDependencies

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
        getPackageDependencies :: (MonadIO m, MonadLogger m)
                               => (Version -> Bool)
                               -> PackageMetadata
                               -> ReaderT
                                      SqlBackend
                                      m
                                      ( Key PkgRecord
                                      , [Category]
                                      , [Version]
                                      , Version
                                      , [(Key PkgRecord, Text, Version)]
                                      )
        getPackageDependencies osPredicate PackageMetadata { packageMetadataPkgRecord = pkg, packageMetadataPkgVersionRecords = pkgVersions, packageMetadataPkgCategories = pkgCategories, packageMetadataPkgVersion = pkgVersion }
            = do
                let pkgId          = entityKey pkg
                let pkgVersions' = versionRecordNumber . entityVal <$> pkgVersions
                let pkgCategories' = entityVal <$> pkgCategories
                pkgDepInfo             <- getPkgDependencyData pkgId pkgVersion
                pkgDepInfoWithVersions <- traverse zipDependencyVersions pkgDepInfo
                let compatiblePkgDepInfo = fmap (filterDependencyOsCompatible osPredicate) pkgDepInfoWithVersions
                res <- catMaybes <$> traverse filterDependencyBestVersion compatiblePkgDepInfo
                pure $ (pkgId, pkgCategories', pkgVersions', pkgVersion, res)
        constructPackageListApiRes :: (MonadResource m, MonadReader r m, Has AppSettings r, Has PkgRepo r)
                                   => ( Key PkgRecord
                                      , [Category]
                                      , [Version]
                                      , Version
                                      , [(Key PkgRecord, Text, Version)]
                                      )
                                   -> m PackageRes
        constructPackageListApiRes (pkgKey, pkgCategories, pkgVersions, pkgVersion, dependencies) = do
            settings <- ask @_ @_ @AppSettings
            let pkgId = unPkgRecordKey pkgKey
            manifest <- flip runReaderT settings $ (snd <$> getManifest pkgId pkgVersion) >>= \bs ->
                runConduit $ bs .| CL.foldMap LBS.fromStrict
            icon <- loadIcon pkgVersion pkgId
            deps <- constructDependenciesApiRes dependencies
            pure $ PackageRes { packageResIcon         = encodeBase64 icon
                        -- pass through raw JSON Value, we have checked its correct parsing above
                              , packageResManifest     = unsafeFromJust . decode $ manifest
                              , packageResCategories   = categoryName <$> pkgCategories
                              , packageResInstructions = basicRender $ InstructionsR pkgId
                              , packageResLicense      = basicRender $ LicenseR pkgId
                              , packageResVersions     = pkgVersions
                              , packageResDependencies = HM.fromList deps
                              }
        constructDependenciesApiRes :: (MonadResource m, MonadReader r m, Has PkgRepo r)
                                    => [(Key PkgRecord, Text, Version)]
                                    -> m [(PkgId, DependencyRes)]
        constructDependenciesApiRes deps = traverse
            (\(depKey, depTitle, depVersion) -> do
                let depId = unPkgRecordKey depKey
                icon <- loadIcon depVersion depId
                pure (depId, DependencyRes { dependencyResTitle = depTitle, dependencyResIcon = encodeBase64 icon })
            )
            deps
        loadIcon :: (Monad m, MonadResource m, MonadReader r m, Has PkgRepo r) => Version -> PkgId -> m ByteString
        loadIcon version pkg = do
            (_, _, src) <- getIcon pkg version
            runConduit $ src .| CL.foldMap id

basicRender :: RenderRoute a => Route a -> Text
basicRender = TL.toStrict . TB.toLazyText . fold . fmap (mappend (TB.singleton '/') . TB.fromText) . fst . renderRoute
