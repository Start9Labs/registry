{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE QuasiQuotes      #-}
{-# LANGUAGE RecordWildCards  #-}
{-# LANGUAGE TemplateHaskell  #-}
{-# LANGUAGE TypeApplications #-}

module Handler.Apps where

import           Startlude

import           Control.Monad.Logger
import           Data.Aeson
import qualified Data.ByteString.Lazy as BS
import           Data.Char
import           Data.Conduit
import qualified Data.Conduit.Binary  as CB
import qualified Data.Text            as T
import qualified GHC.Show             (Show (..))
import           Network.HTTP.Types
import           System.Directory
import           Yesod.Core

import           Foundation
import           Lib.Registry
import           Lib.Semver
import           System.FilePath      ((<.>), (</>))
import           System.Posix.Files   (fileSize, getFileStatus)
import           Settings

pureLog :: Show a => a -> Handler a
pureLog = liftA2 (*>) ($logInfo . show) pure

logRet :: ToJSON a => Handler a -> Handler a
logRet = (>>= liftA2 (*>) ($logInfo . decodeUtf8 . BS.toStrict . encode) pure)

data FileExtension = FileExtension FilePath (Maybe String)
instance Show FileExtension where
    show (FileExtension f Nothing)  = f
    show (FileExtension f (Just e)) = f <.> e

getAppsManifestR :: Handler TypedContent
getAppsManifestR = do
    AppSettings{..} <- appSettings <$> getYesod
    let appResourceDir = resourcesDir </> "apps" </> "apps.yaml"
    respondSource typePlain $ CB.sourceFile appResourceDir .| awaitForever sendChunkBS

getSysR :: Extension "" -> Handler TypedContent
getSysR e = do
    AppSettings{..} <- appSettings <$> getYesod
    let sysResourceDir = resourcesDir </> "sys"
    getApp sysResourceDir e

getAppR :: Extension "s9pk" -> Handler TypedContent
getAppR e = do
    AppSettings{..} <- appSettings <$> getYesod
    let appResourceDir = resourcesDir </> "apps" </> "apps.yaml"
    getApp appResourceDir e

getApp :: KnownSymbol a => FilePath -> Extension a -> Handler TypedContent
getApp rootDir ext = do
    specString <- T.filter (not . isSpace) . fromMaybe "*" <$> lookupGetParam "spec"
    spec <- case readMaybe specString of
        Nothing -> sendResponseStatus status400 ("Invalid App Version Specification" :: Text)
        Just t  -> pure t
    appVersions <- liftIO $ getAvailableAppVersions rootDir ext
    putStrLn $ "valid appversion for " <> (show ext :: String) <> ": " <> show appVersions
    case getSpecifiedAppVersion spec appVersions of
        Nothing -> notFound
        Just (RegisteredAppVersion (_, filePath)) -> do
            exists <- liftIO $ doesFileExist filePath
            if exists
                then do
                    sz <- liftIO $ fileSize <$> getFileStatus filePath
                    addHeader "Content-Length" (show sz)
                    respondSource typePlain $ CB.sourceFile filePath .| awaitForever sendChunkBS
                else notFound


