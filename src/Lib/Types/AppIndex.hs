{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE QuasiQuotes #-}

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
import Data.String.Interpolate.IsString
import qualified Data.ByteString.Lazy as BS

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
    , serviceDependencyInfoVersion :: VersionRange
    , serviceDependencyInfoDescription :: Maybe Text
    , serviceDependencyInfoCritical :: Bool
    } deriving (Show)
instance FromJSON ServiceDependencyInfo where
    parseJSON = withObject "service dependency info" $ \o -> do
        serviceDependencyInfoOptional <- o .:? "optional"
        serviceDependencyInfoVersion <- o .: "version"
        serviceDependencyInfoDescription <- o .:? "description"
        serviceDependencyInfoCritical <- o .: "critical"
        pure ServiceDependencyInfo { .. }
instance ToJSON ServiceDependencyInfo where
    toJSON ServiceDependencyInfo {..} = object
        [ "description" .= serviceDependencyInfoDescription
        , "version" .= serviceDependencyInfoVersion
        , "optional" .= serviceDependencyInfoOptional
        , "critical" .= serviceDependencyInfoCritical
        ]
data ServiceAlert = INSTALL | UNINSTALL | RESTORE | START | STOP
    deriving (Show, Eq, Generic, Hashable, Read)
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
        alerts <- o .: "alerts"
        a <- for (HM.toList alerts) $ \(key, value) -> do
            alertType <- case readMaybe $ T.toUpper key of
                    Nothing -> fail "could not parse alert key as ServiceAlert"
                    Just t -> pure t
            alertDesc <- parseJSON value
            pure (alertType, alertDesc)
        let serviceManifestAlerts = HM.fromList a
        serviceManifestDependencies <- o .: "dependencies"
        pure ServiceManifest { .. }
instance ToJSON ServiceManifest where
    toJSON ServiceManifest {..} = object
        [ "id" .= serviceManifestId
        , "title" .= serviceManifestTitle
        , "version" .= serviceManifestVersion
        , "description" .= object ["short" .= serviceManifestDescriptionShort, "long" .= serviceManifestDescriptionLong]
        , "release-notes" .= serviceManifestReleaseNotes
        , "alerts" .= object [ t .= v | (k,v) <- HM.toList serviceManifestAlerts, let (String t) = toJSON k ]
        , "dependencies" .= serviceManifestDependencies
        ]

-- >>> eitherDecode testManifest :: Either String ServiceManifest
-- Right (ServiceManifest {serviceManifestId = "embassy-pages", serviceManifestTitle = "Embassy Pages", serviceManifestVersion = 0.1.3, serviceManifestDescriptionLong = "Embassy Pages is a simple web server that uses directories inside File Browser to serve Tor websites.", serviceManifestDescriptionShort = "Create Tor websites, hosted on your Embassy.", serviceManifestReleaseNotes = "Upgrade to EmbassyOS v0.3.0", serviceManifestAlerts = fromList [(INSTALL,Nothing),(UNINSTALL,Nothing),(STOP,Nothing),(RESTORE,Nothing),(START,Nothing)], serviceManifestDependencies = fromList [("filebrowser",ServiceDependencyInfo {serviceDependencyInfoOptional = Nothing, serviceDependencyInfoVersion = >=2.14.1.1 <3.0.0, serviceDependencyInfoDescription = Just "Used to upload files to serve.", serviceDependencyInfoCritical = False})]})
testManifest :: BS.ByteString
testManifest = [i|{
  "id": "embassy-pages",
  "title": "Embassy Pages",
  "version": "0.1.3",
  "description": {
    "short": "Create Tor websites, hosted on your Embassy.",
    "long": "Embassy Pages is a simple web server that uses directories inside File Browser to serve Tor websites."
  },
  "assets": {
    "license": "LICENSE",
    "icon": "icon.png",
    "docker-images": "image.tar",
    "instructions": "instructions.md"
  },
  "build": [
    "make"
  ],
  "release-notes": "Upgrade to EmbassyOS v0.3.0",
  "license": "nginx",
  "wrapper-repo": "https://github.com/Start9Labs/embassy-pages-wrapper",
  "upstream-repo": "http://hg.nginx.org/nginx/",
  "support-site": null,
  "marketing-site": null,
  "alerts": {
    "install": null,
    "uninstall": null,
    "restore": null,
    "start": null,
    "stop": null
  },
  "main": {
    "type": "docker",
    "image": "main",
    "system": false,
    "entrypoint": "/usr/local/bin/docker_entrypoint.sh",
    "args": [],
    "mounts": {
      "filebrowser": "/mnt/filebrowser"
    },
    "io-format": "yaml",
    "inject": false,
    "shm-size-mb": null
  },
  "health-checks": {},
  "config": {
    "get": {
      "type": "docker",
      "image": "compat",
      "system": true,
      "entrypoint": "config",
      "args": [
        "get",
        "/root"
      ],
      "mounts": {},
      "io-format": "yaml",
      "inject": false,
      "shm-size-mb": null
    },
    "set": {
      "type": "docker",
      "image": "compat",
      "system": true,
      "entrypoint": "config",
      "args": [
        "set",
        "/root"
      ],
      "mounts": {},
      "io-format": "yaml",
      "inject": false,
      "shm-size-mb": null
    }
  },
  "volumes": {
    "filebrowser": {
      "type": "pointer",
      "package-id": "filebrowser",
      "volume-id": "main",
      "path": "/",
      "readonly": true
    }
  },
  "min-os-version": "0.3.0",
  "interfaces": {
    "main": {
      "tor-config": {
        "port-mapping": {
          "80": "80"
        }
      },
      "lan-config": null,
      "ui": true,
      "protocols": [
        "tcp",
        "http"
      ]
    }
  },
  "backup": {
    "create": {
      "type": "docker",
      "image": "compat",
      "system": true,
      "entrypoint": "true",
      "args": [],
      "mounts": {},
      "io-format": null,
      "inject": false,
      "shm-size-mb": null
    },
    "restore": {
      "type": "docker",
      "image": "compat",
      "system": true,
      "entrypoint": "true",
      "args": [],
      "mounts": {},
      "io-format": null,
      "inject": false,
      "shm-size-mb": null
    }
  },
  "migrations": {
    "from": {},
    "to": {}
  },
  "actions": {},
  "dependencies": {
    "filebrowser": {
      "version": ">=2.14.1.1 <3.0.0",
      "optional": null,
      "description": "Used to upload files to serve.",
      "critical": false,
      "config": null
    }
  }
}|]
