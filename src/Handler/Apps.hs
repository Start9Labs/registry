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
import           System.Directory
import           Yesod.Core

import           Foundation
import           Handler.Types.Status
import           Lib.Registry
import           Lib.Semver
import           Lib.Types.Semver

pureLog :: Show a => a -> Handler a
pureLog = liftA2 (*>) ($logInfo . show) pure

logRet :: ToJSON a => Handler a -> Handler a
logRet = (>>= liftA2 (*>) ($logInfo . decodeUtf8 . BS.toStrict . encode) pure)

getAppsManifestR :: Handler TypedContent
getAppsManifestR = respondSource typePlain $ CB.sourceFile appManifestPath .| awaitForever sendChunkBS

getAgentR :: Handler TypedContent
getAgentR = getApp sysResourceDir $ S9PK "agent"

getAppMgrR :: Handler TypedContent
getAppMgrR = getApp sysResourceDir $ S9PK "appmgr"

getAppR :: S9PK -> Handler TypedContent
getAppR = getApp appResourceDir

getApp :: FilePath -> S9PK -> Handler TypedContent
getApp rootDir (S9PK appId) = do
    spec <- querySpecD mostRecentVersion <$> lookupGetParam "spec"
    appVersions <- registeredAppVersions appId <$> loadRegistry rootDir
    case getSpecifiedAppVersion spec appVersions of
        Nothing -> respondSource typePlain sendFlush
        Just (RegisteredAppVersion (_, filePath)) -> do
            exists <- liftIO $ doesFileExist filePath
            if exists
                then respondSource typePlain $ CB.sourceFile filePath .| awaitForever sendChunkBS
                else respondSource typePlain sendFlush


