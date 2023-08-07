{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Lib.External.AppMgr (
    sourceManifest,
    getPackageHash,
    sourceInstructions,
    sourceLicense,
    sourceIcon,
) where

import Startlude (
    Applicative (pure, (*>)),
    ByteString,
    Eq ((==)),
    FilePath,
    String,
    id,
    stderr,
    throwIO,
    ($),
    (&&),
 )

import Data.ByteString.Lazy qualified as LBS
import Data.String.Interpolate.IsString (
    i,
 )
import System.Process.Typed (
    ExitCodeException (eceExitCode),
    Process,
    ProcessConfig,
    byteStringInput,
    getStdout,
    proc,
    setEnvInherit,
    setStderr,
    setStdin,
    setStdout,
    startProcess,
    stopProcess,
    useHandleOpen,
 )

import Conduit (
    ConduitT,
    runConduit,
    (.|),
 )
import Control.Monad.Logger (
    MonadLoggerIO,
    logErrorSH,
 )
import Data.Conduit.List qualified as CL
import Data.Conduit.Process.Typed (createSource)
import GHC.IO.Exception (
    IOErrorType (NoSuchThing),
    IOException (ioe_description, ioe_type),
 )
import Lib.Error (S9Error (AppMgrE))
import System.FilePath ((</>))
import UnliftIO (
    MonadUnliftIO,
    bracket,
    catch,
 )


readProcessInheritStderr ::
    forall m a.
    MonadUnliftIO m =>
    String ->
    [String] ->
    ByteString ->
    (ConduitT () ByteString m () -> m a) -> -- this is because we can't clean up the process in the unCPS'ed version of this
    m a
readProcessInheritStderr a b c sink = do
    let pc =
            setStdin (byteStringInput $ LBS.fromStrict c) $
                setEnvInherit $
                    setStderr (useHandleOpen stderr) $
                        setStdout createSource $
                            System.Process.Typed.proc a b
    withProcessTerm' pc $ \p -> sink (getStdout p)
    where
        -- We need this to deal with https://github.com/haskell/process/issues/215
        withProcessTerm' ::
            (MonadUnliftIO m) =>
            ProcessConfig stdin stdout stderr ->
            (Process stdin stdout stderr -> m a) ->
            m a
        withProcessTerm' cfg = bracket (startProcess cfg) $ \p -> do
            stopProcess p
                `catch` ( \e ->
                            if ioe_type e == NoSuchThing && ioe_description e == "No child processes"
                                then pure ()
                                else throwIO e
                        )


sourceManifest ::
    (MonadUnliftIO m, MonadLoggerIO m) =>
    FilePath ->
    FilePath ->
    (ConduitT () ByteString m () -> m r) ->
    m r
sourceManifest appmgrPath pkgFile sink = do
    let appmgr = readProcessInheritStderr (appmgrPath </> "start-sdk") ["inspect", "manifest", pkgFile] ""
    appmgr sink `catch` \ece ->
        $logErrorSH ece *> throwIO (AppMgrE [i|start-sdk inspect manifest #{pkgFile}|] (eceExitCode ece))


sourceIcon :: (MonadUnliftIO m, MonadLoggerIO m) => FilePath -> FilePath -> (ConduitT () ByteString m () -> m r) -> m r
sourceIcon appmgrPath pkgFile sink = do
    let appmgr = readProcessInheritStderr (appmgrPath </> "start-sdk") ["inspect", "icon", pkgFile] ""
    appmgr sink `catch` \ece ->
        $logErrorSH ece *> throwIO (AppMgrE [i|start-sdk inspect icon #{pkgFile}|] (eceExitCode ece))


getPackageHash :: (MonadUnliftIO m, MonadLoggerIO m) => FilePath -> FilePath -> m ByteString
getPackageHash appmgrPath pkgFile = do
    let appmgr = readProcessInheritStderr (appmgrPath </> "start-sdk") ["inspect", "hash", pkgFile] ""
    appmgr (\bsSource -> runConduit $ bsSource .| CL.foldMap id) `catch` \ece ->
        $logErrorSH ece *> throwIO (AppMgrE [i|start-sdk inspect hash #{pkgFile}|] (eceExitCode ece))


sourceInstructions ::
    (MonadUnliftIO m, MonadLoggerIO m) =>
    FilePath ->
    FilePath ->
    (ConduitT () ByteString m () -> m r) ->
    m r
sourceInstructions appmgrPath pkgFile sink = do
    let appmgr = readProcessInheritStderr (appmgrPath </> "start-sdk") ["inspect", "instructions", pkgFile] ""
    appmgr sink `catch` \ece ->
        $logErrorSH ece *> throwIO (AppMgrE [i|start-sdk inspect instructions #{pkgFile}|] (eceExitCode ece))


sourceLicense ::
    (MonadUnliftIO m, MonadLoggerIO m) =>
    FilePath ->
    FilePath ->
    (ConduitT () ByteString m () -> m r) ->
    m r
sourceLicense appmgrPath pkgFile sink = do
    let appmgr = readProcessInheritStderr (appmgrPath </> "start-sdk") ["inspect", "license", pkgFile] ""
    appmgr sink `catch` \ece ->
        $logErrorSH ece *> throwIO (AppMgrE [i|start-sdk inspect license #{pkgFile}|] (eceExitCode ece))
