{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Lib.PkgRepository where

import Conduit (
    ConduitT,
    MonadResource,
    runConduit,
    runResourceT,
    sinkFileCautious,
    sourceFile,
    (.|),
 )
import Control.Monad.Logger (
    MonadLogger,
    MonadLoggerIO,
    logError,
    logInfo,
    logWarn,
 )
import Control.Monad.Reader.Has (
    Has,
    ask,
    asks,
 )
import Data.Aeson (eitherDecodeFileStrict')
import Data.Attoparsec.Text qualified as Atto
import Data.ByteString (
    readFile,
    writeFile,
 )
import Data.HashMap.Strict qualified as HM
import Data.String.Interpolate.IsString (
    i,
 )
import Data.Text qualified as T
import Data.Time (getCurrentTime)
import Database.Esqueleto.Experimental (
    ConnectionPool,
    insertUnique,
    runSqlPool,
 )
import Database.Persist (
    insertKey,
    update,
    (=.),
 )
import Database.Persist.Sql (
    SqlPersistT,
    runSqlPoolNoTransaction,
 )
import Database.PostgreSQL.Simple (SqlError (sqlState))
import Lib.Error (S9Error (NotFoundE))
import Lib.External.AppMgr qualified as AppMgr
import Lib.Types.Core (PkgId (..))
import Lib.Types.Emver (
    Version,
    VersionRange,
    parseVersion,
    satisfies,
 )
import Lib.Types.Manifest (
    PackageDependency (..),
    PackageManifest (..),
 )
import Model (
    EntityField (PkgRecordUpdatedAt),
    Key (PkgRecordKey),
    PkgDependency (PkgDependency),
    PkgRecord (PkgRecord),
    VersionRecord (versionRecordNumber),
 )
import Startlude (
    Bool (..),
    ByteString,
    Down (..),
    Either (..),
    Eq ((==)),
    Exception,
    FilePath,
    Integer,
    Maybe (..),
    MonadIO (liftIO),
    MonadReader,
    Ord (compare),
    Show,
    SomeException (..),
    filter,
    find,
    first,
    flip,
    for_,
    fst,
    headMay,
    on,
    partitionEithers,
    pure,
    show,
    snd,
    sortBy,
    throwIO,
    toS,
    ($),
    (.),
    (/=),
    (<$>),
 )

import System.FilePath (
    takeBaseName,
    takeDirectory,
    takeExtension,
    (<.>),
    (</>),
 )
import UnliftIO (
    MonadUnliftIO,
    async,
    catch,
    mapConcurrently_,
    wait,
 )
import UnliftIO.Directory (
    doesDirectoryExist,
    doesPathExist,
    getFileSize,
    listDirectory,
    removeFile,
    renameFile,
 )
import UnliftIO.Exception (handle)
import Yesod.Core.Content (
    typeGif,
    typeJpeg,
    typePlain,
    typePng,
    typeSvg,
 )
import Yesod.Core.Types (ContentType)


newtype ManifestParseException = ManifestParseException FilePath
    deriving (Show)
instance Exception ManifestParseException


data PkgRepo = PkgRepo
    { pkgRepoFileRoot :: !FilePath
    , pkgRepoAppMgrBin :: !FilePath
    }


newtype EosRepo = EosRepo
    { eosRepoFileRoot :: FilePath
    }


getPackages :: (MonadIO m, MonadReader r m, Has PkgRepo r) => m [PkgId]
getPackages = do
    root <- asks pkgRepoFileRoot
    paths <- listDirectory root
    pure $ PkgId . toS <$> paths


getVersionsFor :: (MonadIO m, MonadReader r m, Has PkgRepo r, MonadLogger m) => PkgId -> m [Version]
getVersionsFor pkg = do
    root <- asks pkgRepoFileRoot
    let pkgDir = root </> show pkg
    exists <- doesDirectoryExist pkgDir
    if exists
        then do
            subdirs <- listDirectory pkgDir
            let (failures, successes) = partitionEithers $ Atto.parseOnly parseVersion . T.pack <$> subdirs
            for_ failures $ \f -> $logWarn [i|Emver Parse Failure for #{pkg}: #{f}|]
            pure successes
        else pure []


getViableVersions :: VersionRange -> [VersionRecord] -> [Version]
getViableVersions spec vrs = filter (`satisfies` spec) (versionRecordNumber <$> vrs)


getBestVersion ::
    VersionRange ->
    Bool ->
    [VersionRecord] ->
    (Maybe Version)
getBestVersion spec preferMin vrs = headMay $ sortBy comparator $ getViableVersions spec vrs
    where
        comparator = if preferMin then compare else compare `on` Down


loadPkgDependencies :: MonadUnliftIO m => ConnectionPool -> PackageManifest -> m ()
loadPkgDependencies appConnPool manifest = do
    let pkgId = packageManifestId manifest
    let pkgVersion = packageManifestVersion manifest
    let deps = packageManifestDependencies manifest
    time <- liftIO getCurrentTime
    _ <-
        runWith appConnPool $
            insertKey (PkgRecordKey pkgId) (PkgRecord False time Nothing) `catch` \(e :: SqlError) ->
                -- 23505 is "already exists"
                if sqlState e == "23505" then update (PkgRecordKey pkgId) [PkgRecordUpdatedAt =. Just time] else throwIO e
    let deps' = first PkgRecordKey <$> HM.toList deps
    for_
        deps'
        ( \d -> flip runSqlPool appConnPool $ do
            _ <-
                runWith appConnPool $
                    insertKey (fst d) (PkgRecord False time Nothing) `catch` \(e :: SqlError) ->
                        -- 23505 is "already exists"
                        if sqlState e == "23505" then update (fst d) [PkgRecordUpdatedAt =. Just time] else throwIO e
            insertUnique $
                PkgDependency time (PkgRecordKey pkgId) pkgVersion (fst d) (packageDependencyVersion . snd $ d)
        )
    where
        runWith :: MonadUnliftIO m => ConnectionPool -> SqlPersistT m a -> m a
        runWith pool action = runSqlPoolNoTransaction action pool Nothing


-- extract all package assets into their own respective files
extractPkg :: (MonadUnliftIO m, MonadReader r m, Has PkgRepo r, MonadLoggerIO m) => ConnectionPool -> FilePath -> m ()
extractPkg pool fp = handle @_ @SomeException cleanup $ do
    $logInfo [i|Extracting package: #{fp}|]
    PkgRepo{pkgRepoAppMgrBin = appmgr} <- ask
    let pkgRoot = takeDirectory fp
    manifestTask <- async $ runResourceT $ AppMgr.sourceManifest appmgr fp $ sinkIt (pkgRoot </> "manifest.json")
    pkgHashTask <- async $ AppMgr.getPackageHash appmgr fp
    instructionsTask <-
        async $
            runResourceT $
                AppMgr.sourceInstructions appmgr fp $
                    sinkIt
                        (pkgRoot </> "instructions.md")
    licenseTask <- async $ runResourceT $ AppMgr.sourceLicense appmgr fp $ sinkIt (pkgRoot </> "license.md")
    iconTask <- async $ runResourceT $ AppMgr.sourceIcon appmgr fp $ sinkIt (pkgRoot </> "icon.tmp")
    wait manifestTask
    eManifest <- liftIO (eitherDecodeFileStrict' (pkgRoot </> "manifest.json"))
    case eManifest of
        Left _ -> do
            $logError [i|Invalid Package Manifest: #{fp}|]
            liftIO . throwIO $ ManifestParseException (pkgRoot </> "manifest.json")
        Right manifest -> do
            wait iconTask
            let iconDest =
                    "icon" <.> case packageManifestIcon manifest of
                        Nothing -> "png"
                        Just x -> case takeExtension (T.unpack x) of
                            "" -> "png"
                            other -> other
            loadPkgDependencies pool manifest
            liftIO $ renameFile (pkgRoot </> "icon.tmp") (pkgRoot </> iconDest)
    hash <- wait pkgHashTask
    liftIO $ writeFile (pkgRoot </> "hash.bin") hash
    wait instructionsTask
    wait licenseTask
    where
        sinkIt fp source = runConduit $ source .| sinkFileCautious fp
        cleanup e = do
            $logError $ show e
            let pkgRoot = takeDirectory fp
            fs <- listDirectory pkgRoot
            let toRemove = filter ((/=) ".s9pk" . takeExtension) fs
            mapConcurrently_ (removeFile . (pkgRoot </>)) toRemove
            throwIO e


getManifestLocation :: (MonadReader r m, Has PkgRepo r) => PkgId -> Version -> m FilePath
getManifestLocation pkg version = do
    root <- asks pkgRepoFileRoot
    pure $ root </> show pkg </> show version </> "manifest.json"


getManifest ::
    (MonadResource m, MonadReader r m, Has PkgRepo r) =>
    PkgId ->
    Version ->
    m (Integer, ConduitT () ByteString m ())
getManifest pkg version = do
    manifestPath <- getManifestLocation pkg version
    n <- getFileSize manifestPath
    pure (n, sourceFile manifestPath)


getInstructions ::
    (MonadResource m, MonadReader r m, Has PkgRepo r) =>
    PkgId ->
    Version ->
    m (Integer, ConduitT () ByteString m ())
getInstructions pkg version = do
    root <- asks pkgRepoFileRoot
    let instructionsPath = root </> show pkg </> show version </> "instructions.md"
    n <- getFileSize instructionsPath
    pure (n, sourceFile instructionsPath)


getLicense ::
    (MonadResource m, MonadReader r m, Has PkgRepo r) =>
    PkgId ->
    Version ->
    m (Integer, ConduitT () ByteString m ())
getLicense pkg version = do
    root <- asks pkgRepoFileRoot
    let licensePath = root </> show pkg </> show version </> "license.md"
    n <- getFileSize licensePath
    pure (n, sourceFile licensePath)


getIcon ::
    (MonadResource m, MonadReader r m, Has PkgRepo r) =>
    PkgId ->
    Version ->
    m (ContentType, Integer, ConduitT () ByteString m ())
getIcon pkg version = do
    root <- asks pkgRepoFileRoot
    let pkgRoot = root </> show pkg </> show version
    mIconFile <- find ((== "icon") . takeBaseName) <$> listDirectory pkgRoot
    case mIconFile of
        Nothing -> throwIO $ NotFoundE [i|#{pkg}: Icon|]
        Just x -> do
            let ct = case takeExtension x of
                    ".png" -> typePng
                    ".jpg" -> typeJpeg
                    ".jpeg" -> typeJpeg
                    ".svg" -> typeSvg
                    ".gif" -> typeGif
                    _ -> typePlain
            n <- getFileSize (pkgRoot </> x)
            pure (ct, n, sourceFile (pkgRoot </> x))


getHash :: (MonadIO m, MonadReader r m, Has PkgRepo r) => PkgId -> Version -> m ByteString
getHash pkg version = do
    root <- asks pkgRepoFileRoot
    let hashPath = root </> show pkg </> show version </> "hash.bin"
    liftIO $ readFile hashPath


getPackage :: (MonadResource m, MonadReader r m, Has PkgRepo r) => PkgId -> Version -> m (Maybe FilePath)
getPackage pkg version = do
    root <- asks pkgRepoFileRoot
    let pkgPath = root </> show pkg </> show version </> show pkg <.> "s9pk"
    found <- doesPathExist pkgPath
    pure $ if found then Just pkgPath else Nothing
