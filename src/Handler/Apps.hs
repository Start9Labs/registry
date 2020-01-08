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
import           Data.Conduit
import qualified Data.Conduit.Binary  as CB
import qualified GHC.Show             (Show (..))
import           System.Directory
import           Yesod.Core

import           Foundation
import           Handler.Types.Status
import           Lib.Registry
import           Lib.Semver
import           Lib.Types.Semver
import           System.FilePath      ((<.>))
import           System.Posix.Files   (fileSize, getFileStatus)

pureLog :: Show a => a -> Handler a
pureLog = liftA2 (*>) ($logInfo . show) pure

logRet :: ToJSON a => Handler a -> Handler a
logRet = (>>= liftA2 (*>) ($logInfo . decodeUtf8 . BS.toStrict . encode) pure)

data FileExtension = FileExtension FilePath (Maybe String)
instance Show FileExtension where
    show (FileExtension f Nothing)  = f
    show (FileExtension f (Just e)) = f <.> e

getAppsManifestR :: Handler TypedContent
getAppsManifestR = respondSource typePlain $ CB.sourceFile appManifestPath .| awaitForever sendChunkBS

getSysR :: Extension "" -> Handler TypedContent
getSysR = getApp sysResourceDir

getAppR :: Extension "s9pk" -> Handler TypedContent
getAppR = getApp appResourceDir

getApp :: KnownSymbol a => FilePath -> Extension a -> Handler TypedContent
getApp rootDir ext = do
    spec <- querySpecD mostRecentVersion <$> lookupGetParam "spec"
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


