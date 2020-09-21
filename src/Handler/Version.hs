{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE QuasiQuotes         #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE RecordWildCards  #-}

module Handler.Version where

import           Startlude

import           Control.Monad.Trans.Maybe
import           Data.Char
import qualified Data.Text                     as T
import           Network.HTTP.Types
import           Yesod.Core

import           Foundation
import           Handler.Types.Status
import           Lib.Registry
import           Lib.Semver
import           Lib.Types.Semver
import           Settings
import           System.FilePath                ( (</>) )

getVersionR :: Handler AppVersionRes
getVersionR = do
    rv <- AppVersionRes . registryVersion . appSettings <$> getYesod
    pure . rv $ Nothing

getVersionAppR :: Text -> Handler (Maybe AppVersionRes)
getVersionAppR appId = do
    appsDir <- (</> "apps") . resourcesDir . appSettings <$> getYesod
    getVersionWSpec appsDir appExt
    where appExt = Extension (toS appId) :: Extension "s9pk"

getVersionSysR :: Text -> Handler (Maybe AppVersionRes)
getVersionSysR sysAppId = runMaybeT $ do
    sysDir <- (</> "sys") . resourcesDir . appSettings <$> getYesod
    avr    <- MaybeT $ getVersionWSpec sysDir sysExt
    pure $ avr { appVersionMinCompanion = Just $ AppVersion (1, 1, 0, 0) }
    where sysExt = Extension (toS sysAppId) :: Extension ""

getVersionWSpec :: KnownSymbol a => FilePath -> Extension a -> Handler (Maybe AppVersionRes)
getVersionWSpec rootDir ext = do
    specString <- T.filter (not . isSpace) . fromMaybe "*" <$> lookupGetParam "spec"
    spec       <- case readMaybe specString of
        Nothing -> sendResponseStatus status400 ("Invalid App Version Specification" :: Text)
        Just t  -> pure t
    appVersions <- liftIO $ getAvailableAppVersions rootDir ext
    let av = version <$> getSpecifiedAppVersion spec appVersions
    pure $ liftA2 AppVersionRes av (pure Nothing)
