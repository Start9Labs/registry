{-# LANGUAGE ScopedTypeVariables #-}
module Handler.Version where

import           Startlude

import           Yesod.Core

import           Constants
import           Foundation
import           Handler.Types.Status
import           Lib.Registry
import           Lib.Semver
import           Lib.Types.Semver

getVersionR :: Handler AppVersionRes
getVersionR = pure . AppVersionRes $ registryVersion

getVersionAppR :: Text -> Handler (Maybe AppVersionRes)
getVersionAppR = getVersionWSpec appResourceDir

getVersionAgentR :: Handler (Maybe AppVersionRes)
getVersionAgentR = getVersionWSpec sysResourceDir "agent"

getVersionAppMgrR :: Handler (Maybe AppVersionRes)
getVersionAppMgrR = getVersionWSpec sysResourceDir "appmgr"

getVersionWSpec :: FilePath -> Text -> Handler (Maybe AppVersionRes)
getVersionWSpec rootDir appId = do
    spec <- querySpecD mostRecentVersion <$> lookupGetParam "spec"
    appVersions <- registeredAppVersions (toS appId) <$> loadRegistry rootDir
    pure . fmap (AppVersionRes . version) $ getSpecifiedAppVersion spec appVersions
