{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE RecordWildCards  #-}
{-# LANGUAGE TemplateHaskell  #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Handler.Icons where

import           Startlude               hiding ( Handler )

import           Yesod.Core

import           Data.Aeson
import qualified Data.ByteString.Lazy          as BS
import           Data.Conduit                   ( (.|)
                                                , awaitForever
                                                , runConduit
                                                )
import qualified Data.Conduit.List             as CL
import           Foundation
import           Lib.External.AppMgr
import           Lib.Registry
import           Lib.Types.AppIndex
import           Network.HTTP.Types
import           Settings
import           System.FilePath.Posix
import           Util.Shared

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
    spec                 <- getVersionFromQuery appsDir ext >>= \case
        Nothing -> sendResponseStatus status404 ("Specified App Version Not Found" :: Text)
        Just v  -> pure v
    let appDir = (<> "/") . (</> show spec) . (</> show appId) $ appsDir
    manifest' <- sourceManifest appMgrDir appDir ext (\bsSource -> runConduit $ bsSource .| CL.foldMap BS.fromStrict)
    manifest  <- case eitherDecode manifest' of
        Left e -> do
            $logError "could not parse service manifest!"
            $logError (show e)
            sendResponseStatus status500 ("Internal Server Error" :: Text)
        Right a -> pure a
    mimeType <- case serviceManifestIcon manifest of
        Nothing -> pure typePng
        Just a  -> do
            let (_, iconExt) = splitExtension $ toS a
            let x            = toUpper <$> drop 1 iconExt
            case readMaybe $ toS x of
                Nothing -> do
                    $logInfo $ "unknown icon extension type: " <> show x <> ". Sending back typePlain."
                    pure typePlain
                Just iconType -> case iconType of
                    PNG  -> pure typePng
                    SVG  -> pure typeSvg
                    JPG  -> pure typeJpeg
                    JPEG -> pure typeJpeg
    sourceIcon appMgrDir
               (appDir </> show ext)
               ext
               (\bsSource -> respondSource mimeType (bsSource .| awaitForever sendChunkBS))
    where ext = Extension (show appId) :: Extension "s9pk"

getLicenseR :: AppIdentifier -> Handler TypedContent
getLicenseR appId = do
    (appsDir, appMgrDir) <- getsYesod $ ((</> "apps") . resourcesDir &&& staticBinDir) . appSettings
    spec                 <- getVersionFromQuery appsDir ext >>= \case
        Nothing -> sendResponseStatus status404 ("Specified App Version Not Found" :: Text)
        Just v  -> pure v
    servicePath <- liftIO $ getVersionedFileFromDir appsDir ext spec
    case servicePath of
        Nothing -> notFound
        Just p ->
            sourceLicense appMgrDir p ext (\bsSource -> respondSource typePlain (bsSource .| awaitForever sendChunkBS))
    where ext = Extension (show appId) :: Extension "s9pk"

getInstructionsR :: AppIdentifier -> Handler TypedContent
getInstructionsR appId = do
    (appsDir, appMgrDir) <- getsYesod $ ((</> "apps") . resourcesDir &&& staticBinDir) . appSettings
    spec                 <- getVersionFromQuery appsDir ext >>= \case
        Nothing -> sendResponseStatus status404 ("Specified App Version Not Found" :: Text)
        Just v  -> pure v
    servicePath <- liftIO $ getVersionedFileFromDir appsDir ext spec
    case servicePath of
        Nothing -> notFound
        Just p  -> sourceInstructions appMgrDir
                                      p
                                      ext
                                      (\bsSource -> respondSource typePlain (bsSource .| awaitForever sendChunkBS))
    where ext = Extension (show appId) :: Extension "s9pk"
