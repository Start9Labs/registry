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

import           Startlude               hiding ( catch )

import qualified Data.ByteString.Lazy          as LBS
import           Data.String.Interpolate.IsString
import           System.Process.Typed    hiding ( createPipe )

import           Conduit                        ( (.|)
                                                , ConduitT
                                                , MonadThrow
                                                , runConduit
                                                )
import qualified Data.Conduit.List             as CL
import           Data.Conduit.Process.Typed
import           Lib.Error
import           Lib.Registry
import           System.FilePath                ( (</>) )
import           UnliftIO                       ( MonadUnliftIO
                                                , catch
                                                )

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

readProcessInheritStderr :: MonadUnliftIO m
                         => String
                         -> [String]
                         -> ByteString
                         -> (ConduitT () ByteString m () -> m a) -- this is because we can't clean up the process in the unCPS'ed version of this
                         -> m a
readProcessInheritStderr a b c sink = do
    let pc =
            setStdin (byteStringInput $ LBS.fromStrict c)
                $ setEnvInherit
                $ setStdout createSource
                $ System.Process.Typed.proc a b
    withProcessTerm_ pc $ \p -> sink (getStdout p)

getConfig :: (MonadUnliftIO m, MonadThrow m, KnownSymbol a)
          => FilePath
          -> FilePath
          -> Extension a
          -> (ConduitT () ByteString m () -> m r)
          -> m r
getConfig appmgrPath appPath e@(Extension appId) sink = do
    let
        appmgr = readProcessInheritStderr (appmgrPath <> "embassy-sdk")
                                          ["inspect", "config", appPath </> show e, "--json"]
                                          ""
    appmgr sink `catch` \ece -> throwIO (AppMgrE [i|inspect config #{appId} \--json|] (eceExitCode ece))

getManifest :: (MonadUnliftIO m, KnownSymbol a)
            => FilePath
            -> FilePath
            -> Extension a
            -> (ConduitT () ByteString m () -> m r)
            -> m r
getManifest appmgrPath appPath e@(Extension appId) sink = do
    let appmgr = readProcessInheritStderr (appmgrPath <> "embassy-sdk") ["inspect", "manifest", appPath </> show e] ""
    appmgr sink `catch` \ece -> throwIO (AppMgrE [i|embassy-sdk inspect manifest #{appId}|] (eceExitCode ece))

getIcon :: (MonadUnliftIO m, KnownSymbol a)
        => FilePath
        -> FilePath
        -> Extension a
        -> (ConduitT () ByteString m () -> m r)
        -> m r
getIcon appmgrPath appPath (Extension icon) sink = do
    let appmgr = readProcessInheritStderr (appmgrPath <> "embassy-sdk") ["inspect", "icon", appPath] ""
    appmgr sink `catch` \ece -> throwIO $ AppMgrE [i|embassy-sdk inspect icon #{icon}|] (eceExitCode ece)

getPackageHash :: (MonadUnliftIO m, KnownSymbol a) => FilePath -> FilePath -> Extension a -> m ByteString
getPackageHash appmgrPath appPath e@(Extension appId) = do
    let appmgr = readProcessInheritStderr (appmgrPath <> "embassy-sdk") ["inspect", "hash", appPath <> show e] ""
    appmgr (\bsSource -> runConduit $ bsSource .| CL.foldMap id)
        `catch` \ece -> throwIO $ AppMgrE [i|embassy-sdk inspect hash #{appId}|] (eceExitCode ece)

getInstructions :: (MonadUnliftIO m, KnownSymbol a)
                => FilePath
                -> FilePath
                -> Extension a
                -> (ConduitT () ByteString m () -> m r)
                -> m r
getInstructions appmgrPath appPath (Extension appId) sink = do
    let appmgr = readProcessInheritStderr (appmgrPath <> "embassy-sdk") ["inspect", "instructions", appPath] ""
    appmgr sink `catch` \ece -> throwIO $ AppMgrE [i|embassy-sdk inspect instructions #{appId}|] (eceExitCode ece)

getLicense :: (MonadUnliftIO m, KnownSymbol a)
           => FilePath
           -> FilePath
           -> Extension a
           -> (ConduitT () ByteString m () -> m r)
           -> m r
getLicense appmgrPath appPath (Extension appId) sink = do
    let appmgr = readProcessInheritStderr (appmgrPath <> "embassy-sdk") ["inspect", "license", appPath] ""
    appmgr sink `catch` \ece -> throwIO $ AppMgrE [i|embassy-sdk inspect license #{appId}|] (eceExitCode ece)

sinkMem :: (Monad m, Monoid a) => ConduitT () a m () -> m a
sinkMem c = runConduit $ c .| CL.foldMap id
