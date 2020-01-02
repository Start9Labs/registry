{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE TypeApplications #-}

module Handler.Icons where

import           Startlude

import           Data.Conduit
import qualified Data.Conduit.Binary as CB
import           System.Directory
import           Yesod.Core

import           Foundation
import           Lib.Registry

getIconsR :: Extension "png" -> Handler TypedContent
getIconsR ext = do
    mPng <- liftIO $ getUnversionedFileFromDir iconsResourceDir ext
    case mPng of
        Nothing -> notFound
        Just pngPath -> do
            putStrLn @Text $ show pngPath
            exists <- liftIO $ doesFileExist pngPath
            if exists
                then respondSource typePlain $ CB.sourceFile pngPath .| awaitForever sendChunkBS
                else notFound
