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

pureLog :: Show a => a -> Handler a
pureLog = liftA2 (*>) ($logInfo . show) pure

logRet :: ToJSON a => Handler a -> Handler a
logRet = (>>= liftA2 (*>) ($logInfo . decodeUtf8 . BS.toStrict . encode) pure)

data FileExtension = FileExtension FilePath (Maybe String)
instance Show FileExtension where
    show (FileExtension f Nothing)  = f
    show (FileExtension f (Just e)) = f <.> e

getImageR :: Handler TypedContent
getImageR = getApp sysResourceDir "image"

getAppsManifestR :: Handler TypedContent
getAppsManifestR = respondSource typePlain $ CB.sourceFile appManifestPath .| awaitForever sendChunkBS

getAgentR :: Handler TypedContent
getAgentR = getApp sysResourceDir "agent"

getAppMgrR :: Handler TypedContent
getAppMgrR = getApp sysResourceDir "appmgr"

getTorrcR :: Handler TypedContent
getTorrcR = getApp sysResourceDir "torrc"

getAppR :: S9PK -> Handler TypedContent
getAppR (S9PK appId) = getApp appResourceDir appId

getApp :: FilePath -> FilePath -> Handler TypedContent
getApp rootDir appId = do
    spec <- querySpecD mostRecentVersion <$> lookupGetParam "spec"
    reg <- loadRegistry rootDir
    putStrLn ("got registry" :: String)
    let appVersions = registeredAppVersions appId reg
    putStrLn $ "valid appversion for " <> appId <> ": " <> show (fmap version appVersions)
    case getSpecifiedAppVersion spec appVersions of
        Nothing -> respondSource typePlain sendFlush
        Just (RegisteredAppVersion (_, filePath)) -> do
            exists <- liftIO $ doesFileExist filePath
            if exists
                then respondSource typePlain $ CB.sourceFile filePath .| awaitForever sendChunkBS
                else respondSource typePlain sendFlush


