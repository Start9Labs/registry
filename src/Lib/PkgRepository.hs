{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DataKinds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
module Lib.PkgRepository where

import           Conduit                        ( (.|)
                                                , ConduitT
                                                , MonadResource
                                                , runConduit
                                                , runResourceT
                                                , sinkFileCautious
                                                , sourceFile
                                                )
import           Control.Monad.Logger           ( MonadLogger
                                                , MonadLoggerIO
                                                , logError
                                                , logInfo
                                                , logWarn
                                                )
import           Control.Monad.Reader.Has       ( Has
                                                , ask
                                                , asks
                                                )
import           Data.Aeson                     ( eitherDecodeFileStrict' )
import qualified Data.Attoparsec.Text          as Atto
import           Data.String.Interpolate.IsString
                                                ( i )
import qualified Data.Text                     as T
import           Lib.Error                      ( S9Error(NotFoundE) )
import qualified Lib.External.AppMgr           as AppMgr
import           Lib.Types.AppIndex             ( PkgId(..)
                                                , ServiceManifest(serviceManifestIcon)
                                                )
import           Lib.Types.Emver                ( Version
                                                , parseVersion
                                                )
import           Startlude                      ( ($)
                                                , (&&)
                                                , (.)
                                                , (<$>)
                                                , (<>)
                                                , Bool(..)
                                                , ByteString
                                                , Either(Left, Right)
                                                , Eq((==))
                                                , Exception
                                                , FilePath
                                                , IO
                                                , Maybe(Just, Nothing)
                                                , MonadIO(liftIO)
                                                , MonadReader
                                                , Show
                                                , filter
                                                , find
                                                , for_
                                                , fromMaybe
                                                , not
                                                , partitionEithers
                                                , pure
                                                , show
                                                , throwIO
                                                )
import           System.FSNotify                ( Event(Added)
                                                , eventPath
                                                , watchTree
                                                , withManager
                                                )
import           System.FilePath                ( (<.>)
                                                , (</>)
                                                , takeBaseName
                                                , takeDirectory
                                                , takeExtension
                                                )
import           UnliftIO                       ( MonadUnliftIO
                                                , askRunInIO
                                                , async
                                                , mapConcurrently
                                                , newEmptyMVar
                                                , onException
                                                , takeMVar
                                                , wait
                                                )
import           UnliftIO                       ( tryPutMVar )
import           UnliftIO.Concurrent            ( forkIO )
import           UnliftIO.Directory             ( listDirectory
                                                , removeFile
                                                , renameFile
                                                )
import           Yesod.Core.Content             ( typeGif
                                                , typeJpeg
                                                , typePlain
                                                , typePng
                                                , typeSvg
                                                )
import           Yesod.Core.Types               ( ContentType )

data ManifestParseException = ManifestParseException FilePath
    deriving Show
instance Exception ManifestParseException

data PkgRepo = PkgRepo
    { pkgRepoFileRoot  :: FilePath
    , pkgRepoAppMgrBin :: FilePath
    }

getVersionsFor :: (MonadIO m, MonadReader r m, Has PkgRepo r, MonadLogger m) => PkgId -> m [Version]
getVersionsFor pkg = do
    root    <- asks pkgRepoFileRoot
    subdirs <- listDirectory $ root </> show pkg
    let (failures, successes) = partitionEithers $ (Atto.parseOnly parseVersion . T.pack) <$> subdirs
    for_ failures $ \f -> $logWarn [i|Emver Parse Failure for #{pkg}: #{f}|]
    pure successes

-- extract all package assets into their own respective files
extractPkg :: (MonadUnliftIO m, MonadReader r m, Has PkgRepo r, MonadLoggerIO m) => FilePath -> m ()
extractPkg fp = (`onException` cleanup) $ do
    $logInfo [i|Extracting package: #{fp}|]
    PkgRepo { pkgRepoAppMgrBin = appmgr } <- ask
    let pkgRoot = takeDirectory fp
    -- let s9pk    = pkgRoot </> show pkg <.> "s9pk"
    manifestTask <- async $ liftIO . runResourceT $ AppMgr.sourceManifest appmgr fp $ sinkIt
        (pkgRoot </> "manifest.json")
    instructionsTask <- async $ liftIO . runResourceT $ AppMgr.sourceInstructions appmgr fp $ sinkIt
        (pkgRoot </> "instructions.md")
    licenseTask <- async $ liftIO . runResourceT $ AppMgr.sourceLicense appmgr fp $ sinkIt (pkgRoot </> "license.md")
    iconTask    <- async $ liftIO . runResourceT $ AppMgr.sourceIcon appmgr fp $ sinkIt (pkgRoot </> "icon.tmp")
    wait manifestTask
    eManifest <- liftIO (eitherDecodeFileStrict' (pkgRoot </> "manifest.json"))
    case eManifest of
        Left _ -> do
            $logError [i|Invalid Package Manifest: #{fp}|]
            liftIO . throwIO $ ManifestParseException (pkgRoot </> "manifest.json")
        Right manifest -> do
            wait iconTask
            let iconDest = "icon" <.> T.unpack (fromMaybe "png" (serviceManifestIcon manifest))
            liftIO $ renameFile (pkgRoot </> "icon.tmp") (pkgRoot </> iconDest)
    wait instructionsTask
    wait licenseTask
    where
        sinkIt fp source = runConduit $ source .| sinkFileCautious fp
        cleanup = do
            let pkgRoot = takeDirectory fp
            fs <- listDirectory pkgRoot
            let toRemove = filter (not . (== ".s9pk") . takeExtension) fs
            mapConcurrently (removeFile . (pkgRoot </>)) toRemove

watchPkgRepoRoot :: (MonadUnliftIO m, MonadReader r m, Has PkgRepo r, MonadLoggerIO m) => m (IO Bool)
watchPkgRepoRoot = do
    root    <- asks pkgRepoFileRoot
    runInIO <- askRunInIO
    box     <- newEmptyMVar @_ @()
    _       <- forkIO $ liftIO $ withManager $ \watchManager -> do
        stop <- watchTree watchManager root onlyAdded $ \evt -> do
            let pkg = eventPath evt
            -- TODO: validate that package path is an actual s9pk and is in a correctly conforming path.
            runInIO (extractPkg pkg)
        takeMVar box
        stop
    pure $ tryPutMVar box ()
    where
        onlyAdded = \case
            Added path _ isDir -> not isDir && takeExtension path == ".s9pk"
            _                  -> False

getManifest :: (MonadResource m, MonadReader r m, Has PkgRepo r) => PkgId -> Version -> ConduitT () ByteString m ()
getManifest pkg version = do
    root <- asks pkgRepoFileRoot
    let manifestPath = root </> show pkg </> show version </> "manifest.json"
    sourceFile manifestPath

getInstructions :: (MonadResource m, MonadReader r m, Has PkgRepo r) => PkgId -> Version -> ConduitT () ByteString m ()
getInstructions pkg version = do
    root <- asks pkgRepoFileRoot
    let instructionsPath = root </> show pkg </> show version </> "instructions.md"
    sourceFile instructionsPath

getLicense :: (MonadResource m, MonadReader r m, Has PkgRepo r) => PkgId -> Version -> ConduitT () ByteString m ()
getLicense pkg version = do
    root <- asks pkgRepoFileRoot
    let licensePath = root </> show pkg </> show version </> "license.md"
    sourceFile licensePath

getIcon :: (MonadResource m, MonadReader r m, Has PkgRepo r)
        => PkgId
        -> Version
        -> m (ContentType, ConduitT () ByteString m ())
getIcon pkg version = do
    root <- asks pkgRepoFileRoot
    let pkgRoot = root </> show pkg </> show version
    mIconFile <- find ((== "icon") . takeBaseName) <$> listDirectory pkgRoot
    case mIconFile of
        Nothing -> throwIO $ NotFoundE $ show pkg <> ": Icon"
        Just x  -> do
            let ct = case takeExtension x of
                    ".png"  -> typePng
                    ".jpg"  -> typeJpeg
                    ".jpeg" -> typeJpeg
                    ".svg"  -> typeSvg
                    ".gif"  -> typeGif
                    _       -> typePlain
            pure $ (ct, sourceFile (pkgRoot </> x))
