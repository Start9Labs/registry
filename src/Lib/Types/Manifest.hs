{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}

module Lib.Types.Manifest where

import Control.Monad.Fail (MonadFail (..))
import Data.HashMap.Internal.Strict (HashMap)
import Data.HashMap.Strict qualified as HM
import Data.String.Interpolate.IsString (i)
import Data.Text qualified as T
import Lib.Types.Core (PkgId, OsArch)
import Lib.Types.Emver (Version (..), VersionRange)
import Startlude (ByteString, Eq, Generic, Hashable, Maybe (..), Monad ((>>=)), Read, Show, Text, for, pure, readMaybe, ($), Int, (.), fmap, String, null, (<$>), otherwise)
import Data.Aeson
    ( eitherDecodeStrict,
      (.:),
      (.:?),
      withObject,
      withText,
      FromJSON(parseJSON),
      ToJSON(toJSON), encode )
import Database.Persist.Sql ( PersistFieldSql(..) )
import Database.Persist.Types (SqlType(..))
import Database.Persist (PersistValue(..))
import Data.Either (Either(..))
import Database.Persist.Class ( PersistField(..) )
import Data.Maybe (maybe)
import qualified Data.ByteString as BS
import Yesod.Persist (LiteralType(Escaped))
import Data.Aeson.Types (Value(Object), Parser)


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
    , packageHardwareArch :: !(Maybe [OsArch])
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
        packageHardwareDevice <- o .:? "hardware-requirements" >>= maybe (pure Nothing) (.:? "device") >>= parsePackageDevice
        packageHardwareRam <- o .:? "hardware-requirements" >>= maybe (pure Nothing) (.:? "ram")
        packageHardwareArch <- o .:? "hardware-requirements" >>= maybe (pure Nothing)  (.:? "arch")
        pure PackageManifest{..}

parsePackageDevice :: Maybe Value -> Parser (Maybe PackageDevice)
parsePackageDevice (Just (Object o))
    | null o = pure Nothing
    | otherwise = Just . PackageDevice <$> parseJSON (Object o)
parsePackageDevice _ = pure Nothing

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

newtype PackageDevice = PackageDevice { unPackageDevice :: HashMap Text RegexPattern }
  deriving (Show, Eq, Generic)

instance FromJSON PackageDevice where
    parseJSON = fmap PackageDevice . parseJSON
instance ToJSON PackageDevice where
    toJSON = toJSON . unPackageDevice

instance PersistField PackageDevice where
    toPersistValue =  PersistLiteral_ Escaped . BS.toStrict . encode
    fromPersistValue (PersistByteString t) = case eitherDecodeStrict t of
        Left err -> Left $ T.pack err
        Right val -> Right val
    fromPersistValue _ = Left "Invalid JSON value in database ERR"

instance PersistFieldSql PackageDevice where
    sqlType _ = SqlOther "JSONB"
data ServiceAlert = INSTALL | UNINSTALL | RESTORE | START | STOP
    deriving (Show, Eq, Generic, Hashable, Read)


-- >>> eitherDecodeStrict testManifest :: Either String PackageManifest
-- Right (PackageManifest {packageManifestId = embassy-pages, packageManifestTitle = "Embassy Pages", packageManifestVersion = 0.1.3, packageManifestDescriptionLong = "Embassy Pages is a simple web server that uses directories inside File Browser to serve Tor websites.", packageManifestDescriptionShort = "Create Tor websites, hosted on your Embassy.", packageManifestReleaseNotes = "Upgrade to EmbassyOS v0.3.0", packageManifestIcon = Just "icon.png", packageManifestAlerts = fromList [(STOP,Nothing),(RESTORE,Nothing),(INSTALL,Nothing),(START,Nothing),(UNINSTALL,Nothing)], packageManifestDependencies = fromList [(filebrowser,PackageDependency {packageDependencyOptional = Nothing, packageDependencyVersion = >=2.14.1.1 <3.0.0, packageDependencyDescription = Just "Used to upload files to serve."})], packageManifestEosVersion = 0.3.0, packageHardwareDevice = Just (PackageDevice {unPackageDevice = fromList [("processor",RegexPattern "^[A-Za-z0-9]+$"),("display",RegexPattern "CometLake")]}), packageHardwareRam = Just 8000000000, packageHardwareArch = Just [aarch64,x86_64]})
testManifest :: ByteString
testManifest =
    [i|{
  "id": "embassy-pages",
  "title": "Embassy Pages",
  "version": "0.1.3",
  "eos-version": "0.3.0",
  "description": {
    "short": "Create Tor websites, hosted on your Embassy.",
    "long": "Embassy Pages is a simple web server that uses directories inside File Browser to serve Tor websites."
  },
  "hardware-requirements": {
    "device": {
      "processor": "^[A-Za-z0-9]+$",
      "display": "CometLake"
    },
    "ram": 8000000000,
    "arch": ["aarch64", "x86_64"]
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
