{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE RecordWildCards  #-}
{-# LANGUAGE TemplateHaskell  #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables #-}

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
import Data.Aeson
import System.FilePath.Posix

data IconType = PNG | JPG | JPEG | SVG
    deriving (Eq, Show, Generic, Read)
instance ToJSON IconType
instance FromJSON IconType

-- >>> readMaybe $ ixt :: Maybe IconType
-- Just PNG
ixt :: Text
ixt = toS $ toUpper <$> drop 1 ".png"

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
            manifest' <- handleS9ErrT $ getManifest appMgrDir appsDir ext
            manifest <- case eitherDecode $ BS.fromStrict manifest' of
                    Left e -> do
                        $logError "could not parse service manifest!"
                        $logError (show e)
                        sendResponseStatus status500 ("Internal Server Error" :: Text)
                    Right a -> pure a
            mimeType <- case serviceManifestIcon manifest of
                    Nothing -> pure typePng 
                    Just a -> do
                        let (_, iconExt) = splitExtension $ toS a
                        let x = toUpper <$> drop 1 iconExt
                        case readMaybe $ toS x of
                            Nothing -> do
                                $logInfo $ "unknown icon extension type: " <> show x <> ". Sending back typePlain."
                                pure typePlain
                            Just iconType -> case iconType of
                                PNG -> pure typePng 
                                SVG -> pure typeSvg 
                                JPG -> pure typeJpeg 
                                JPEG -> pure typeJpeg 
            respondSource mimeType (sendChunkBS =<< handleS9ErrT (getIcon appMgrDir p ext))
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
