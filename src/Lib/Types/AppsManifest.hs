{-# LANGUAGE RecordWildCards #-}

module Lib.Types.AppsManifest where

import           Data.HashMap.Strict
import           Data.Yaml

import           Lib.Types.Semver
import           Startlude


type AppsManifest = HashMap Text FullAppManifest
data FullAppManifest = FullAppManifest { name :: Text, appGlobals :: AppGlobals, appVersionDetails :: NonEmpty AppVersionDetails }

data AppGlobals = AppGlobals
    { globalAppTitle            :: Text
    , globalAppDescriptionShort :: Text
    , globalAppDescriptionLong  :: Text
    , globalAppIconUrl          :: Text
    }

instance FromJSON AppGlobals where
    parseJSON = withObject "App Globals" $ \o -> do
        desc <- o .: "description"
        (globalAppDescriptionShort, globalAppDescriptionLong) <-
            ( withObject "App Description" $ \d -> do
                s <- d .: "short"
                l <- d .: "long"
                pure (s,l)
            ) desc
        globalAppTitle <- o .: "title"
        globalAppIconUrl <- o .: "icon-url"
        pure AppGlobals{..}
instance ToJSON AppGlobals where
    toJSON AppGlobals{..} = object
        [ "title" .= globalAppTitle
        , "descriptionShort" .= globalAppDescriptionShort
        , "descriptionLong" .= globalAppDescriptionLong
        , "iconUrl" .= globalAppIconUrl
        ]


data AppVersionDetails = AppVersionDetails
    { versionDetailsVersion      :: AppVersion
    , versionDetailsReleaseNotes :: Text
    } deriving (Eq, Ord, Show)
instance FromJSON AppVersionDetails where
    parseJSON = withObject "Store App Version Info" $ \o -> do
        versionDetailsVersion <- o .: "version"
        versionDetailsReleaseNotes <- o .: "release-notes"
        pure AppVersionDetails{..}
instance ToJSON AppVersionDetails where
    toJSON AppVersionDetails{..} = object
        [ "version" .= versionDetailsVersion
        , "releaseNotes" .= versionDetailsReleaseNotes
        ]
