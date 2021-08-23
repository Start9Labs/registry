{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE RecordWildCards  #-}

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
import qualified Data.ByteString.Lazy as BS

getIconsR :: Extension "png" -> Handler TypedContent
getIconsR ext = do
    (appsDir, appMgrDir) <- getsYesod $ ((</> "apps") . resourcesDir &&& staticBinDir) . appSettings
    spec <- getVersionFromQuery appsDir ext >>= \case 
        Nothing -> notFound 
        Just v -> pure v
    servicePath <- liftIO $ getVersionedFileFromDir (appsDir </> show spec) ext spec
    case servicePath of
        Nothing -> notFound
        Just p -> do
            icon <- handleS9ErrT $ getIcon appMgrDir p ext
            respondSource typePlain $ CB.sourceLazy (BS.fromStrict icon) .| awaitForever sendChunkBS

-- getLicenseR :: Extension "" -> Handler TypedContent
-- getLicenseR ext = do
--     AppSettings{..} <- appSettings <$> getYesod
--     mPng <- liftIO $ getUnversionedFileFromDir (resourcesDir </> "icons") ext
--     case mPng of
--         Nothing -> notFound
--         Just pngPath -> do
--             putStrLn @Text $ show pngPath
--             exists <- liftIO $ doesFileExist pngPath
--             if exists
--                 then respondSource typePlain $ CB.sourceFile pngPath .| awaitForever sendChunkBS
--                 else notFound

-- getMarkdownR :: Extension "md" -> Handler TypedContent
-- getMarkdownR ext = do
--     -- @TODO switch to getting from service directory
--     AppSettings{..} <- appSettings <$> getYesod
--     mPng <- liftIO $ getUnversionedFileFromDir (resourcesDir </> "icons") ext
--     case mPng of
--         Nothing -> notFound
--         Just pngPath -> do
--             putStrLn @Text $ show pngPath
--             exists <- liftIO $ doesFileExist pngPath
--             if exists
--                 then respondSource typePlain $ CB.sourceFile pngPath .| awaitForever sendChunkBS
--                 else notFound