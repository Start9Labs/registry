{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
module Lib.Types.AppIndex where

import           Startlude

import           Control.Monad.Fail
import           Data.Aeson
import qualified Data.HashMap.Strict           as HM
import qualified Data.List.NonEmpty            as NE

import           Lib.Semver
import           Lib.Types.Semver

type AppIdentifier = Text

data VersionInfo = VersionInfo
    { versionInfoVersion       :: AppVersion
    , versionInfoReleaseNotes  :: Text
    , versionInfoDependencies  :: HM.HashMap Text AppVersionSpec
    , versionInfoOsRequired    :: AppVersionSpec
    , versionInfoOsRecommended :: AppVersionSpec
    }
    deriving (Eq, Show)

instance Ord VersionInfo where
    compare = compare `on` versionInfoVersion

instance FromJSON VersionInfo where
    parseJSON = withObject "version info" $ \o -> do
        versionInfoVersion       <- o .: "version"
        versionInfoReleaseNotes  <- o .: "release-notes"
        versionInfoDependencies  <- o .:? "dependencies" .!= HM.empty
        versionInfoOsRequired    <- o .:? "os-version-required" .!= AppVersionAny
        versionInfoOsRecommended <- o .:? "os-version-recommended" .!= AppVersionAny
        pure VersionInfo { .. }

instance ToJSON VersionInfo where
    toJSON VersionInfo {..} = object
        [ "version" .= versionInfoVersion
        , "release-notes" .= versionInfoReleaseNotes
        , "dependencies" .= versionInfoDependencies
        , "os-version-required" .= versionInfoOsRequired
        , "os-version-recommended" .= versionInfoOsRecommended
        ]

data StoreApp = StoreApp
    { storeAppTitle       :: Text
    , storeAppDescShort   :: Text
    , storeAppDescLong    :: Text
    , storeAppVersionInfo :: NonEmpty VersionInfo
    , storeAppIconType    :: Text
    }
    deriving Show

instance ToJSON StoreApp where
    toJSON StoreApp {..} = object
        [ "title" .= storeAppTitle
        , "icon-type" .= storeAppIconType
        , "description" .= object ["short" .= storeAppDescShort, "long" .= storeAppDescLong]
        , "version-info" .= storeAppVersionInfo
        ]

newtype AppManifest = AppManifest { unAppManifest :: HM.HashMap AppIdentifier StoreApp}
    deriving (Show)

instance FromJSON AppManifest where
    parseJSON = withObject "app details to seed" $ \o -> do
        apps <- for (HM.toList o) $ \(appId', c) -> do
            appId               <- parseJSON $ String appId'
            config              <- parseJSON c
            storeAppTitle       <- config .: "title"
            storeAppIconType    <- config .: "icon-type"
            storeAppDescShort   <- config .: "description" >>= (.: "short")
            storeAppDescLong    <- config .: "description" >>= (.: "long")
            storeAppVersionInfo <- config .: "version-info" >>= \case
                []       -> fail "No Valid Version Info"
                (x : xs) -> pure $ x :| xs
            return $ (appId, StoreApp { .. })
        return $ AppManifest (HM.fromList apps)
instance ToJSON AppManifest where
    toJSON = toJSON . unAppManifest


filterOsRequired :: AppVersion -> StoreApp -> Maybe StoreApp
filterOsRequired av sa = case NE.filter ((av <||) . versionInfoOsRequired) (storeAppVersionInfo sa) of
    []       -> Nothing
    (x : xs) -> Just $ sa { storeAppVersionInfo = x :| xs }

filterOsRecommended :: AppVersion -> StoreApp -> Maybe StoreApp
filterOsRecommended av sa = case NE.filter ((av <||) . versionInfoOsRecommended) (storeAppVersionInfo sa) of
    []       -> Nothing
    (x : xs) -> Just $ sa { storeAppVersionInfo = x :| xs }
