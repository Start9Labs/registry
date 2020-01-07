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

getVersionSysR :: Text -> Handler (Maybe AppVersionRes)
getVersionSysR sysAppId = getVersionWSpec sysResourceDir sysExt
    where
        sysExt = Extension (toS sysAppId) :: Extension ""

getVersionWSpec :: KnownSymbol a => FilePath -> Extension a -> Handler (Maybe AppVersionRes)
getVersionWSpec rootDir ext = do
    spec <- querySpecD mostRecentVersion <$> lookupGetParam "spec"
    appVersions <- liftIO $ getAvailableAppVersions rootDir ext
    pure . fmap (AppVersionRes . version) $ getSpecifiedAppVersion spec appVersions
