{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE RecordWildCards  #-}
{-# LANGUAGE TemplateHaskell  #-}

module Handler.Icons where

import           Startlude hiding (Handler)

import           Data.Conduit
import qualified Data.Conduit.Combinators as CB
import           System.Directory
import           Yesod.Core

import           Foundation
import           Lib.Registry
import           Settings
import           System.FilePath ((</>))
import Util.Shared
import Lib.External.AppMgr
import Lib.Error
import Data.Conduit.Process
import Conduit
import qualified Data.ByteString.Lazy as BS
import Network.HTTP.Types
import Lib.Types.AppIndex

getIconsR :: AppIdentifier -> Handler TypedContent
getIconsR appId = do
    (appsDir, appMgrDir) <- getsYesod $ ((</> "apps") . resourcesDir &&& staticBinDir) . appSettings
    $logInfo $ show ext
    spec <- getVersionFromQuery appsDir ext >>= \case
        Nothing -> sendResponseStatus status404 ("Specified App Version Not Found" :: Text)
        Just v  -> pure v
    servicePath <- liftIO $ getVersionedFileFromDir appsDir ext spec
    case servicePath of
        Nothing -> notFound
        Just p -> do
            -- (_, Just hout, _, _) <- liftIO (createProcess $ iconBs { std_out = CreatePipe })
            -- respondSource typePlain (runConduit $ yieldMany () [iconBs])
            -- respondSource typePlain $ sourceHandle hout .| awaitForever sendChunkBS
            respondSource typePlain (sendChunkBS =<< handleS9ErrT (getIcon appMgrDir p ext))
    where ext = Extension (toS appId) :: Extension "s9pk"

getLicenseR :: AppIdentifier -> Handler TypedContent
getLicenseR appId = do
    (appsDir, appMgrDir) <- getsYesod $ ((</> "apps") . resourcesDir &&& staticBinDir) . appSettings
    $logInfo $ show ext
    spec <- getVersionFromQuery appsDir ext >>= \case
        Nothing -> sendResponseStatus status404 ("Specified App Version Not Found" :: Text)
        Just v  -> pure v
    servicePath <- liftIO $ getVersionedFileFromDir appsDir ext spec
    case servicePath of
        Nothing -> notFound
        Just p -> do
            respondSource typePlain (sendChunkBS =<< handleS9ErrT (getLicense appMgrDir p ext))
    where ext = Extension (toS appId) :: Extension "s9pk"

getInstructionsR :: AppIdentifier -> Handler TypedContent
getInstructionsR appId = do
    (appsDir, appMgrDir) <- getsYesod $ ((</> "apps") . resourcesDir &&& staticBinDir) . appSettings
    $logInfo $ show ext
    spec <- getVersionFromQuery appsDir ext >>= \case
        Nothing -> sendResponseStatus status404 ("Specified App Version Not Found" :: Text)
        Just v  -> pure v
    servicePath <- liftIO $ getVersionedFileFromDir appsDir ext spec
    case servicePath of
        Nothing -> notFound
        Just p -> do
            respondSource typePlain (sendChunkBS =<< handleS9ErrT (getInstructions appMgrDir p ext))
    where ext = Extension (toS appId) :: Extension "s9pk"