{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
module Lib.Types.AppIndex where

import           Startlude               hiding ( Any )

import           Control.Monad.Fail
import           Data.Aeson
import qualified Data.HashMap.Strict           as HM
import qualified Data.List.NonEmpty            as NE

import           Lib.Types.Emver
import           Orphans.Emver                  ( )
import           System.Directory
import           Lib.Registry
import Model
import qualified Data.Text as T

type AppIdentifier = Text

data VersionInfo = VersionInfo
    { versionInfoVersion       :: Version
    , versionInfoReleaseNotes  :: Text
    , versionInfoDependencies  :: HM.HashMap AppIdentifier VersionRange
    , versionInfoOsRequired    :: VersionRange
    , versionInfoOsRecommended :: VersionRange
    , versionInfoInstallAlert  :: Maybe Text
    }
    deriving (Eq, Show)

mapSVersionToVersionInfo :: [SVersion] -> [VersionInfo]
mapSVersionToVersionInfo sv = do
    (\v -> VersionInfo {
      versionInfoVersion = sVersionNumber v
    , versionInfoReleaseNotes = sVersionReleaseNotes v
    , versionInfoDependencies = HM.empty
    , versionInfoOsRequired = sVersionOsVersionRequired v
    , versionInfoOsRecommended = sVersionOsVersionRecommended v
    , versionInfoInstallAlert = Nothing
    }) <$> sv

instance Ord VersionInfo where
    compare = compare `on` versionInfoVersion

instance FromJSON VersionInfo where
    parseJSON = withObject "version info" $ \o -> do
        versionInfoVersion       <- o .: "version"
        versionInfoReleaseNotes  <- o .: "release-notes"
        versionInfoDependencies  <- o .:? "dependencies" .!= HM.empty
        versionInfoOsRequired    <- o .:? "os-version-required" .!= Any
        versionInfoOsRecommended <- o .:? "os-version-recommended" .!= Any
        versionInfoInstallAlert  <- o .:? "install-alert"
        pure VersionInfo { .. }

instance ToJSON VersionInfo where
    toJSON VersionInfo {..} = object
        [ "version" .= versionInfoVersion
        , "release-notes" .= versionInfoReleaseNotes
        , "dependencies" .= versionInfoDependencies
        , "os-version-required" .= versionInfoOsRequired
        , "os-version-recommended" .= versionInfoOsRecommended
        , "install-alert" .= versionInfoInstallAlert
        ]

data StoreApp = StoreApp
    { storeAppTitle       :: Text
    , storeAppDescShort   :: Text
    , storeAppDescLong    :: Text
    , storeAppVersionInfo :: NonEmpty VersionInfo
    , storeAppIconType    :: Text
    , storeAppTimestamp   :: Maybe UTCTime
    }
    deriving Show

instance ToJSON StoreApp where
    toJSON StoreApp {..} = object
        [ "title" .= storeAppTitle
        , "icon-type" .= storeAppIconType
        , "description" .= object ["short" .= storeAppDescShort, "long" .= storeAppDescLong]
        , "version-info" .= storeAppVersionInfo
        , "timestamp" .= storeAppTimestamp
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
            storeAppTimestamp   <- config .:? "timestamp"
            pure (appId, StoreApp { .. })
        return $ AppManifest (HM.fromList apps)
instance ToJSON AppManifest where
    toJSON = toJSON . unAppManifest

filterOsRequired :: Version -> StoreApp -> Maybe StoreApp
filterOsRequired av sa = case NE.filter ((av <||) . versionInfoOsRequired) (storeAppVersionInfo sa) of
    []       -> Nothing
    (x : xs) -> Just $ sa { storeAppVersionInfo = x :| xs }

filterOsRecommended :: Version -> StoreApp -> Maybe StoreApp
filterOsRecommended av sa = case NE.filter ((av <||) . versionInfoOsRecommended) (storeAppVersionInfo sa) of
    []       -> Nothing
    (x : xs) -> Just $ sa { storeAppVersionInfo = x :| xs }

addFileTimestamp :: KnownSymbol a => FilePath -> Extension a -> StoreApp -> Version -> IO (Maybe StoreApp)
addFileTimestamp appDir ext service v = do
    getVersionedFileFromDir appDir ext v >>= \case
                Nothing -> pure Nothing
                Just file -> do
                    time <- getModificationTime file
                    pure $ Just service {storeAppTimestamp = Just time }

data ServiceDependencyInfo = ServiceDependencyInfo
    { serviceDependencyInfoOptional :: Maybe Text
    , serviceDependencyInfoRecommended :: Bool 
    , serviceDependencyInfoVersion :: Version
    , serviceDependencyInfoDescription :: Maybe Text
    } deriving (Show)
instance FromJSON ServiceDependencyInfo where
    parseJSON = withObject "service dependency info" $ \o -> do
        serviceDependencyInfoOptional <- o .:? "optional"
        serviceDependencyInfoRecommended <- o .: "recommended"
        serviceDependencyInfoVersion <- o .: "version"
        serviceDependencyInfoDescription <- o .:? "description"
        pure ServiceDependencyInfo { .. }

instance ToJSON ServiceDependencyInfo where
    toJSON ServiceDependencyInfo {..} = object
        [ "description" .= serviceDependencyInfoDescription
        , "version" .= serviceDependencyInfoVersion
        , "recommended" .= serviceDependencyInfoRecommended
        , "optional" .= serviceDependencyInfoOptional
        ]
data ServiceAlert = INSTALL | UNINSTALL | RESTORE | START | STOP
    deriving (Show, Eq, Generic, Hashable)
instance FromJSONKey ServiceAlert
instance ToJSONKey ServiceAlert
instance ToJSON ServiceAlert where
    toJSON = String . T.toLower . show
instance FromJSON ServiceAlert where
    parseJSON = withText "ServiceAlert" $ \case
        "install"   -> pure INSTALL
        "uninstall" -> pure UNINSTALL
        "restore"   -> pure RESTORE
        "start"     -> pure START
        "stop"      -> pure STOP
        _           -> fail "unknown service alert type"
data ServiceManifest = ServiceManifest 
    { serviceManifestId :: AppIdentifier
    , serviceManifestTitle :: Text
    , serviceManifestVersion :: Version
    , serviceManifestDescriptionLong :: Text
    , serviceManifestDescriptionShort :: Text
    , serviceManifestReleaseNotes :: Text
    , serviceManifestAlerts :: HM.HashMap ServiceAlert (Maybe Text)
    , serviceManifestDependencies :: HM.HashMap AppIdentifier ServiceDependencyInfo
    } deriving (Show)
instance FromJSON ServiceManifest where
    parseJSON = withObject "service manifest" $ \o -> do
        serviceManifestId <- o .: "id"
        serviceManifestTitle <- o .: "title"
        serviceManifestVersion <- o .: "version"
        serviceManifestDescriptionLong <- o .: "description" >>= (.: "long")
        serviceManifestDescriptionShort <- o .: "description" >>= (.: "short")
        serviceManifestReleaseNotes <- o .: "release-notes"
        serviceManifestAlerts <- o .: "alerts"
        serviceManifestDependencies <- o .: "dependencies"
        pure ServiceManifest { .. }
instance ToJSON ServiceManifest where
    toJSON ServiceManifest {..} = object
        [ "id" .= serviceManifestId
        , "title" .= serviceManifestTitle
        , "version" .= serviceManifestVersion
        , "description" .= object ["short" .= serviceManifestDescriptionShort, "long" .= serviceManifestDescriptionLong]
        , "release-notes" .= serviceManifestReleaseNotes
        , "alerts" .= serviceManifestAlerts
        , "dependencies" .= serviceManifestDependencies
        ]