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
                                                , runConduit
                                                , runResourceT
                                                , sinkFileCautious
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
import qualified Lib.External.AppMgr           as AppMgr
import           Lib.Registry                   ( Extension(Extension) )
import           Lib.Types.AppIndex             ( PkgId(PkgId)
                                                , ServiceManifest(serviceManifestIcon)
                                                )
import           Lib.Types.Emver                ( Version
                                                , parseVersion
                                                )
import           Startlude                      ( ($)
                                                , (&&)
                                                , (.)
                                                , (<$>)
                                                , Bool(..)
                                                , Either(Left, Right)
                                                , Eq((==))
                                                , Exception
                                                , FilePath
                                                , IO
                                                , MonadIO(liftIO)
                                                , MonadReader
                                                , Show
                                                , String
                                                , filter
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
                                                , takeFileName
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
            runInIO (extractPkg pkg)
        takeMVar box
        stop
    pure $ tryPutMVar box ()
    where
        onlyAdded = \case
            Added path _ isDir -> not isDir && takeExtension path == ".s9pk"
            _                  -> False
