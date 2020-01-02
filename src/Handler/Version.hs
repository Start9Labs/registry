{-# LANGUAGE DataKinds           #-}
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
getVersionAppR appId = getVersionWSpec appResourceDir appExt
    where
        appExt = Extension (toS appId) :: Extension "s9pk"

getVersionAgentR :: Handler (Maybe AppVersionRes)
getVersionAgentR = getVersionWSpec sysResourceDir ("agent" :: Extension "")

getVersionAppMgrR :: Handler (Maybe AppVersionRes)
getVersionAppMgrR = getVersionWSpec sysResourceDir ("appmgr" :: Extension "")

getVersionTorrcR :: Handler (Maybe AppVersionRes)
getVersionTorrcR = getVersionWSpec sysResourceDir ("torrc" :: Extension "")

getVersionWSpec :: KnownSymbol a => FilePath -> Extension a -> Handler (Maybe AppVersionRes)
getVersionWSpec rootDir ext = do
    spec <- querySpecD mostRecentVersion <$> lookupGetParam "spec"
    appVersions <- liftIO $ getAvailableAppVersions rootDir ext
    pure . fmap (AppVersionRes . version) $ getSpecifiedAppVersion spec appVersions
