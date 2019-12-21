{-# LANGUAGE RecordWildCards #-}
module Handler.Types.Apps where

import           Startlude

import           Data.Aeson
import           Data.Time.ISO8601
import           Yesod.Core.Content

import           Lib.Types.Semver
import           Lib.Types.ServerApp

newtype AvailableAppsRes = AvailableAppsRes
    { availableApps :: [(StoreApp, Maybe AppVersion)]
    } deriving (Eq, Show)
instance ToJSON AvailableAppsRes where
    toJSON = toJSON . fmap toJSON' . availableApps
        where
            toJSON' (StoreApp{..}, version) = object
                [ "id" .= storeAppId
                , "title" .= storeAppTitle
                , "versionInstalled" .= version
                , "versionLatest" .= (storeAppVersionInfoVersion . extract) storeAppVersions
                , "iconURL" .= storeAppIconUrl
                , "descriptionShort" .= storeAppDescriptionShort
                ]
instance ToTypedContent AvailableAppsRes where
    toTypedContent = toTypedContent . toJSON
instance ToContent AvailableAppsRes where
    toContent = toContent . toJSON

newtype AvailableAppFullRes = AvailableAppFullRes
    { availableAppFull :: (StoreApp, Maybe AppVersion)
    } deriving (Eq, Show)
instance ToJSON AvailableAppFullRes where
    toJSON = toJSON' . availableAppFull
        where
            toJSON' (StoreApp{..}, version) = object
                [ "id" .= storeAppId
                , "title" .= storeAppTitle
                , "versionInstalled" .= version
                , "versionLatest" .= (storeAppVersionInfoVersion . extract) storeAppVersions
                , "iconURL" .= storeAppIconUrl
                , "descriptionShort" .= storeAppDescriptionShort
                , "descriptionLong" .= storeAppDescriptionLong
                , "versions" .= storeAppVersions
                ]
instance ToContent AvailableAppFullRes where
    toContent = toContent . toJSON
instance ToTypedContent AvailableAppFullRes where
    toTypedContent = toTypedContent . toJSON

newtype InstalledAppRes = InstalledAppRes
    { installedApp :: (StoreApp, ServerApp, AppStatus, UTCTime)
    } deriving (Eq, Show)
instance ToJSON InstalledAppRes where
    toJSON = toJSON' . installedApp
        where
            toJSON' (store, server, status, time) = object
                [ "id" .= storeAppId store
                , "title" .= storeAppTitle store
                , "versionLatest" .= (storeAppVersionInfoVersion . extract) (storeAppVersions store)
                , "versionInstalled" .= serverAppVersionInstalled server
                , "iconURL" .= storeAppIconUrl store
                , "torAddress" .= serverAppTorService server
                , "status" .= status
                , "statusAt" .= formatISO8601Javascript time
                ]
instance ToTypedContent InstalledAppRes where
    toTypedContent = toTypedContent . toJSON
instance ToContent InstalledAppRes where
    toContent = toContent . toJSON

data InstallNewAppReq = InstallNewAppReq
    { installNewAppId      :: Text
    , installNewAppVersion :: Text
    } deriving (Eq, Show)
instance FromJSON InstallNewAppReq where
    parseJSON = withObject "Install New App Request" $ \o -> do
        installNewAppId <- o .: "id"
        installNewAppVersion <- o .: "version"
        pure InstallNewAppReq{..}
