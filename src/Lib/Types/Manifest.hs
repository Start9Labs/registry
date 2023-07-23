{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}

module Lib.Types.Manifest where

import Control.Monad.Fail (MonadFail (..))
import Data.HashMap.Internal.Strict (HashMap)
import Data.HashMap.Strict qualified as HM
import Data.String.Interpolate.IsString (i)
import Data.Text qualified as T
import Lib.Types.Core (PkgId)
import Lib.Types.Emver (Version (..), VersionRange)
import Startlude (ByteString, Eq, Generic, Hashable, Maybe (..), Monad ((>>=)), Read, Show, Text, for, pure, readMaybe, ($), Int, (.), (<>))
import Data.Aeson
import Database.Persist.Sql ( PersistFieldSql(..) )
import Database.Persist.Types (SqlType(..))
import qualified Data.Text.Encoding as TE
import qualified Data.ByteString.Lazy as BL
import Database.Persist (PersistValue(..))
import Data.Either (Either(..))
import Database.Persist.Class ( PersistField(..) )

data PackageManifest = PackageManifest
    { packageManifestId :: !PkgId
    , packageManifestTitle :: !Text
    , packageManifestVersion :: !Version
    , packageManifestDescriptionLong :: !Text
    , packageManifestDescriptionShort :: !Text
    , packageManifestReleaseNotes :: !Text
    , packageManifestIcon :: !(Maybe Text)
    , packageManifestAlerts :: !(HashMap ServiceAlert (Maybe Text))
    , packageManifestDependencies :: !(HashMap PkgId PackageDependency)
    , packageManifestEosVersion :: !Version
    , packageHardwareDevice :: !(Maybe PackageDevice)
    , packageHardwareRam :: !(Maybe Int)
    }
    deriving (Show)
instance FromJSON PackageManifest where
    parseJSON = withObject "service manifest" $ \o -> do
        packageManifestId <- o .: "id"
        packageManifestTitle <- o .: "title"
        packageManifestVersion <- o .: "version"
        packageManifestDescriptionLong <- o .: "description" >>= (.: "long")
        packageManifestDescriptionShort <- o .: "description" >>= (.: "short")
        packageManifestIcon <- o .: "assets" >>= (.: "icon")
        packageManifestReleaseNotes <- o .: "release-notes"
        alerts <- o .: "alerts"
        a <- for (HM.toList alerts) $ \(key, value) -> do
            alertType <- case readMaybe $ T.toUpper key of
                Nothing -> fail "could not parse alert key as ServiceAlert"
                Just t -> pure t
            alertDesc <- parseJSON value
            pure (alertType, alertDesc)
        let packageManifestAlerts = HM.fromList a
        packageManifestDependencies <- o .: "dependencies"
        packageManifestEosVersion <- o .: "eos-version"
        packageHardwareDevice <- o .: "hardware-requirements" >>= (.: "device")
        packageHardwareRam <- o .: "hardware-requirements" >>= (.: "ram")
        pure PackageManifest{..}


data PackageDependency = PackageDependency
    { packageDependencyOptional :: !(Maybe Text)
    , packageDependencyVersion :: !VersionRange
    , packageDependencyDescription :: !(Maybe Text)
    }
    deriving (Show)
instance FromJSON PackageDependency where
    parseJSON = withObject "service dependency info" $ \o -> do
        packageDependencyOptional <- o .:? "optional"
        packageDependencyVersion <- o .: "version"
        packageDependencyDescription <- o .:? "description"
        pure PackageDependency{..}

-- Custom type for regex pattern
newtype RegexPattern = RegexPattern Text
  deriving (Show, Eq, Generic)

instance FromJSON RegexPattern where
    parseJSON = withText "RegexPattern" (pure . RegexPattern)
instance ToJSON RegexPattern where
    toJSON (RegexPattern txt) = toJSON txt

data PackageDevice = PackageDevice (HashMap Text RegexPattern)
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

instance PersistField PackageDevice where
    toPersistValue = PersistByteString . BL.toStrict . encode
    fromPersistValue (PersistByteString bs) = case eitherDecode (BL.fromStrict bs) of
        Left err -> Left $ TE.decodeUtf8 bs <> ": " <> T.pack err
        Right val -> Right val
    fromPersistValue _ = Left "Invalid JSON value in database"

instance PersistFieldSql PackageDevice where
    sqlType _ = SqlOther "JSONB"
data ServiceAlert = INSTALL | UNINSTALL | RESTORE | START | STOP
    deriving (Show, Eq, Generic, Hashable, Read)


-- >>> eitherDecode testManifest :: Either String PackageManifest
testManifest :: ByteString
testManifest =
    [i|{
  "id": "embassy-pages",
  "title": "Embassy Pages",
  "version": "0.1.3",
  "description": {
    "short": "Create Tor websites, hosted on your Embassy.",
    "long": "Embassy Pages is a simple web server that uses directories inside File Browser to serve Tor websites."
  },
  "hardware-requirements" {
    "device": {
      "processor": "intel",
      "display": "r'^{.*}$'"
    },
    "ram": "8"
  }
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