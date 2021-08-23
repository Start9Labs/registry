{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE RecordWildCards  #-}

module Handler.Icons where

import           Startlude hiding (Handler)

import           Data.Conduit
import qualified Data.Conduit.Binary as CB
import           System.Directory
import           Yesod.Core

import           Foundation
import           Lib.Registry
import           Settings
import           System.FilePath ((</>))

getIconsR :: Extension "png" -> Handler TypedContent
getIconsR ext = do
    -- @TODO switch to getting from service directory
    AppSettings{..} <- appSettings <$> getYesod
    mPng <- liftIO $ getVersionedFileFromDir (resourcesDir </> "icons") ext
    case mPng of
        Nothing -> notFound
        Just pngPath -> do
            putStrLn @Text $ show pngPath
            exists <- liftIO $ doesFileExist pngPath
            if exists
                then respondSource typePlain $ CB.sourceFile pngPath .| awaitForever sendChunkBS
                else notFound

getLicenseR :: Extension "" -> Handler TypedContent
getLicenseR ext = do
    AppSettings{..} <- appSettings <$> getYesod
    mPng <- liftIO $ getUnversionedFileFromDir (resourcesDir </> "icons") ext
    case mPng of
        Nothing -> notFound
        Just pngPath -> do
            putStrLn @Text $ show pngPath
            exists <- liftIO $ doesFileExist pngPath
            if exists
                then respondSource typePlain $ CB.sourceFile pngPath .| awaitForever sendChunkBS
                else notFound

getMarkdownR :: Extension "md" -> Handler TypedContent
getMarkdownR ext = do
    -- @TODO switch to getting from service directory
    AppSettings{..} <- appSettings <$> getYesod
    mPng <- liftIO $ getUnversionedFileFromDir (resourcesDir </> "icons") ext
    case mPng of
        Nothing -> notFound
        Just pngPath -> do
            putStrLn @Text $ show pngPath
            exists <- liftIO $ doesFileExist pngPath
            if exists
                then respondSource typePlain $ CB.sourceFile pngPath .| awaitForever sendChunkBS
                else notFound