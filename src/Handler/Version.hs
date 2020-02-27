{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE QuasiQuotes         #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
module Handler.Version where

import           Startlude

import           Control.Monad.Trans.Maybe
import           Data.Char
import           Data.String.Interpolate.IsString
import qualified Data.Text                        as T
import           Network.HTTP.Types
import           Yesod.Core

import           Constants
import           Foundation
import           Handler.Types.Status
import           Lib.Registry
import           Lib.Semver
import           Lib.Types.Semver

getVersionR :: Handler AppVersionRes
getVersionR = pure . AppVersionRes registryVersion $ Nothing

getVersionAppR :: Text -> Handler (Maybe AppVersionRes)
getVersionAppR appId = getVersionWSpec appResourceDir appExt
    where
        appExt = Extension (toS appId) :: Extension "s9pk"

getVersionSysR :: Text -> Handler (Maybe AppVersionRes)
getVersionSysR sysAppId = runMaybeT $ do
    avr <- MaybeT $ getVersionWSpec sysResourceDir sysExt
    minComp <- lift $ case sysAppId of
        "agent" -> Just <$> meshCompanionCompatibility (appVersionVersion avr)
        _       -> pure Nothing
    pure $ avr { appVersionMinCompanion = minComp }
    where
        sysExt = Extension (toS sysAppId) :: Extension ""

getVersionWSpec :: KnownSymbol a => FilePath -> Extension a -> Handler (Maybe AppVersionRes)
getVersionWSpec rootDir ext = do
    specString <- T.filter (not . isSpace) . fromMaybe "*" <$> lookupGetParam "spec"
    spec <- case readMaybe specString of
        Nothing -> sendResponseStatus status400 ("Invalid App Version Specification" :: Text)
        Just t  -> pure t
    appVersions <- liftIO $ getAvailableAppVersions rootDir ext
    let av = version <$> getSpecifiedAppVersion spec appVersions
    pure $ liftA2 AppVersionRes av (pure Nothing)

meshCompanionCompatibility :: AppVersion -> Handler AppVersion
meshCompanionCompatibility (AppVersion (0,1,0,_)) = pure $ AppVersion (1,0,0,0)
meshCompanionCompatibility (AppVersion (0,1,1,_)) = pure $ AppVersion (1,0,0,0)
meshCompanionCompatibility (AppVersion (0,1,2,_)) = pure $ AppVersion (1,1,0,0)
meshCompanionCompatibility other = do
    $logError [i|MESH DEPLOYMENT "#{other}" HAS NO COMPATIBILITY ENTRY! FIX IMMEDIATELY|]
    sendResponseStatus status500 ("Internal Server Error" :: Text)
