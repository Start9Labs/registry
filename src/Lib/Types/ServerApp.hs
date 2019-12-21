
{-# LANGUAGE RecordWildCards #-}
module Lib.Types.ServerApp where

import           Startlude

import           Control.Monad.Fail
import           Data.Aeson
import           Data.Text

import           Lib.Types.Semver

data StoreApp = StoreApp
    { storeAppId               :: Text
    , storeAppTitle            :: Text
    , storeAppDescriptionShort :: Text
    , storeAppDescriptionLong  :: Text
    , storeAppIconUrl          :: Text
    , storeAppVersions         :: NonEmpty StoreAppVersionInfo
    } deriving (Eq, Show)

data StoreAppVersionInfo = StoreAppVersionInfo
    { storeAppVersionInfoVersion      :: AppVersion
    , storeAppVersionInfoReleaseNotes :: Text
    } deriving (Eq, Ord, Show)
instance FromJSON StoreAppVersionInfo where
    parseJSON = withObject "Store App Version Info" $ \o -> do
        storeAppVersionInfoVersion <- o .: "version"
        storeAppVersionInfoReleaseNotes <- o .: "release-notes"
        pure StoreAppVersionInfo{..}
instance ToJSON StoreAppVersionInfo where
    toJSON StoreAppVersionInfo{..} = object
        [ "version" .= storeAppVersionInfoVersion
        , "releaseNotes" .= storeAppVersionInfoReleaseNotes
        ]

data ServerApp = ServerApp
    { serverAppId               :: Text
    , serverAppVersionInstalled :: AppVersion
    , serverAppTorService       :: Text
    , serverAppIsConfigured     :: Bool
    } deriving (Eq, Show)


data AppStatus = Running | Stopped | Restarting | Removing | Dead deriving (Eq, Show)
instance ToJSON AppStatus where
    toJSON = String . toUpper . show
instance FromJSON AppStatus where
    parseJSON = withText "health status" $ \case
        "RUNNING" -> pure Running
        "STOPPED" -> pure Stopped
        "RESTARTING" -> pure Restarting
        "REMOVING" -> pure Removing
        "DEAD" -> pure Dead
        _         -> fail "unknown status"

data AppAction = Start | Stop deriving (Eq, Show)
