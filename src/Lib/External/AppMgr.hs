{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE GADTs            #-}
{-# LANGUAGE QuasiQuotes      #-}
{-# LANGUAGE RecordWildCards  #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies     #-}
{-# LANGUAGE BangPatterns #-}

module Lib.External.AppMgr where

import           Startlude               hiding ( bracket
                                                , catch
                                                , finally
                                                , handle
                                                )

import qualified Data.ByteString.Lazy          as LBS
import           Data.String.Interpolate.IsString
import           System.Process.Typed    hiding ( createPipe )

import           Conduit                        ( (.|)
                                                , ConduitT
                                                , runConduit
                                                )
import qualified Data.Conduit.List             as CL
import           Data.Conduit.Process.Typed
import           GHC.IO.Exception               ( IOErrorType(NoSuchThing)
                                                , IOException(ioe_description, ioe_type)
                                                )
import           Lib.Error
import           System.FilePath                ( (</>) )
import           UnliftIO                       ( MonadUnliftIO
                                                , catch
                                                )
import           UnliftIO                       ( bracket )

readProcessWithExitCode' :: MonadIO m => String -> [String] -> ByteString -> m (ExitCode, ByteString, ByteString)
readProcessWithExitCode' a b c = liftIO $ do
    let pc =
            setStdin (byteStringInput $ LBS.fromStrict c)
                $ setStderr byteStringOutput
                $ setEnvInherit
                $ setStdout byteStringOutput
                $ System.Process.Typed.proc a b
    withProcessWait pc $ \process -> atomically $ liftA3 (,,)
                                                         (waitExitCodeSTM process)
                                                         (LBS.toStrict <$> getStdout process)
                                                         (LBS.toStrict <$> getStderr process)

readProcessInheritStderr :: forall m a
                          . MonadUnliftIO m
                         => String
                         -> [String]
                         -> ByteString
                         -> (ConduitT () ByteString m () -> m a) -- this is because we can't clean up the process in the unCPS'ed version of this
                         -> m a
readProcessInheritStderr a b c sink = do
    let pc =
            setStdin (byteStringInput $ LBS.fromStrict c)
                $ setEnvInherit
                $ setStderr (useHandleOpen stderr)
                $ setStdout createSource
                $ System.Process.Typed.proc a b
    withProcessTerm' pc $ \p -> sink (getStdout p)
    where
        -- We need this to deal with https://github.com/haskell/process/issues/215
        withProcessTerm' :: (MonadUnliftIO m)
                         => ProcessConfig stdin stdout stderr
                         -> (Process stdin stdout stderr -> m a)
                         -> m a
        withProcessTerm' cfg = bracket (startProcess cfg) $ \p -> do
            stopProcess p
                `catch` (\e -> if ioe_type e == NoSuchThing && ioe_description e == "No child processes"
                            then pure ()
                            else throwIO e
                        )

sourceManifest :: (MonadUnliftIO m) => FilePath -> FilePath -> (ConduitT () ByteString m () -> m r) -> m r
sourceManifest appmgrPath pkgFile sink = do
    let appmgr = readProcessInheritStderr (appmgrPath </> "embassy-sdk") ["inspect", "manifest", pkgFile] ""
    appmgr sink `catch` \ece ->
        print ece *> throwIO (AppMgrE [i|embassy-sdk inspect manifest #{pkgFile}|] (eceExitCode ece))

sourceIcon :: (MonadUnliftIO m) => FilePath -> FilePath -> (ConduitT () ByteString m () -> m r) -> m r
sourceIcon appmgrPath pkgFile sink = do
    let appmgr = readProcessInheritStderr (appmgrPath </> "embassy-sdk") ["inspect", "icon", pkgFile] ""
    appmgr sink
        `catch` \ece -> print ece *> throwIO (AppMgrE [i|embassy-sdk inspect icon #{pkgFile}|] (eceExitCode ece))

getPackageHash :: (MonadUnliftIO m) => FilePath -> FilePath -> m ByteString
getPackageHash appmgrPath pkgFile = do
    let appmgr = readProcessInheritStderr (appmgrPath </> "embassy-sdk") ["inspect", "hash", pkgFile] ""
    appmgr (\bsSource -> runConduit $ bsSource .| CL.foldMap id)
        `catch` \ece -> print ece *> throwIO (AppMgrE [i|embassy-sdk inspect hash #{pkgFile}|] (eceExitCode ece))

sourceInstructions :: (MonadUnliftIO m) => FilePath -> FilePath -> (ConduitT () ByteString m () -> m r) -> m r
sourceInstructions appmgrPath pkgFile sink = do
    let appmgr = readProcessInheritStderr (appmgrPath </> "embassy-sdk") ["inspect", "instructions", pkgFile] ""
    appmgr sink `catch` \ece ->
        print ece *> throwIO (AppMgrE [i|embassy-sdk inspect instructions #{pkgFile}|] (eceExitCode ece))

sourceLicense :: (MonadUnliftIO m) => FilePath -> FilePath -> (ConduitT () ByteString m () -> m r) -> m r
sourceLicense appmgrPath pkgFile sink = do
    let appmgr = readProcessInheritStderr (appmgrPath </> "embassy-sdk") ["inspect", "license", pkgFile] ""
    appmgr sink
        `catch` \ece -> print ece *> throwIO (AppMgrE [i|embassy-sdk inspect license #{pkgFile}|] (eceExitCode ece))

sinkMem :: (Monad m, Monoid a) => ConduitT () a m () -> m a
sinkMem c = runConduit $ c .| CL.foldMap id
