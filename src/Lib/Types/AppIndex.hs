{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE QuasiQuotes #-}
module Lib.Types.AppIndex where

import           Startlude

-- NOTE: leave eitherDecode for inline test evaluation below
import           Control.Monad                  ( fail )
import           Data.Aeson                     ( (.:)
                                                , (.:?)
                                                , FromJSON(..)
                                                , FromJSONKey(..)
                                                , ToJSON(..)
                                                , ToJSONKey(..)
                                                , withObject
                                                )
import qualified Data.ByteString.Lazy          as BS
import           Data.Functor.Contravariant     ( contramap )
import qualified Data.HashMap.Strict           as HM
import           Data.String.Interpolate.IsString
import qualified Data.Text                     as T
import           Database.Persist               ( PersistField(..)
                                                , PersistValue(PersistText)
                                                , SqlType(..)
                                                )
import           Database.Persist.Sql           ( PersistFieldSql(sqlType) )
import           GHC.Read                       ( Read(readsPrec) )
import           Lib.Types.Emver                ( Version
                                                , VersionRange
                                                )
import           Orphans.Emver                  ( )
import qualified Protolude.Base                as P
                                                ( Show(..) )
import           Web.HttpApiData                ( FromHttpApiData
                                                , ToHttpApiData
                                                )
import           Yesod                          ( PathPiece(..) )
newtype PkgId = PkgId { unPkgId :: Text }
    deriving stock (Eq, Ord)
    deriving newtype (FromHttpApiData, ToHttpApiData)
instance IsString PkgId where
    fromString = PkgId . fromString
instance P.Show PkgId where
    show = toS . unPkgId
instance Read PkgId where
    readsPrec _ s = [(PkgId $ toS s, "")]
instance Hashable PkgId where
    hashWithSalt n = hashWithSalt n . unPkgId
instance FromJSON PkgId where
    parseJSON = fmap PkgId . parseJSON
instance ToJSON PkgId where
    toJSON = toJSON . unPkgId
instance FromJSONKey PkgId where
    fromJSONKey = fmap PkgId fromJSONKey
instance ToJSONKey PkgId where
    toJSONKey = contramap unPkgId toJSONKey
instance PersistField PkgId where
    toPersistValue = PersistText . show
    fromPersistValue (PersistText t) = Right . PkgId $ toS t
    fromPersistValue other           = Left [i|Invalid AppId: #{other}|]
instance PersistFieldSql PkgId where
    sqlType _ = SqlString
instance PathPiece PkgId where
    fromPathPiece = fmap PkgId . fromPathPiece
    toPathPiece   = unPkgId
data VersionInfo = VersionInfo
    { versionInfoVersion      :: Version
    , versionInfoReleaseNotes :: Text
    , versionInfoDependencies :: HM.HashMap PkgId VersionRange
    , versionInfoOsVersion    :: Version
    , versionInfoInstallAlert :: Maybe Text
    }
    deriving (Eq, Show)

data PackageDependency = PackageDependency
    { packageDependencyOptional    :: Maybe Text
    , packageDependencyVersion     :: VersionRange
    , packageDependencyDescription :: Maybe Text
    }
    deriving Show
instance FromJSON PackageDependency where
    parseJSON = withObject "service dependency info" $ \o -> do
        packageDependencyOptional    <- o .:? "optional"
        packageDependencyVersion     <- o .: "version"
        packageDependencyDescription <- o .:? "description"
        pure PackageDependency { .. }
data ServiceAlert = INSTALL | UNINSTALL | RESTORE | START | STOP
    deriving (Show, Eq, Generic, Hashable, Read)
data PackageManifest = PackageManifest
    { packageManifestId               :: !PkgId
    , packageManifestTitle            :: !Text
    , packageManifestVersion          :: !Version
    , packageManifestDescriptionLong  :: !Text
    , packageManifestDescriptionShort :: !Text
    , packageManifestReleaseNotes     :: !Text
    , packageManifestIcon             :: !(Maybe Text)
    , packageManifestAlerts           :: !(HM.HashMap ServiceAlert (Maybe Text))
    , packageManifestDependencies     :: !(HM.HashMap PkgId PackageDependency)
    }
    deriving Show
instance FromJSON PackageManifest where
    parseJSON = withObject "service manifest" $ \o -> do
        packageManifestId               <- o .: "id"
        packageManifestTitle            <- o .: "title"
        packageManifestVersion          <- o .: "version"
        packageManifestDescriptionLong  <- o .: "description" >>= (.: "long")
        packageManifestDescriptionShort <- o .: "description" >>= (.: "short")
        packageManifestIcon             <- o .: "assets" >>= (.: "icon")
        packageManifestReleaseNotes     <- o .: "release-notes"
        alerts                          <- o .: "alerts"
        a                               <- for (HM.toList alerts) $ \(key, value) -> do
            alertType <- case readMaybe $ T.toUpper key of
                Nothing -> fail "could not parse alert key as ServiceAlert"
                Just t  -> pure t
            alertDesc <- parseJSON value
            pure (alertType, alertDesc)
        let packageManifestAlerts = HM.fromList a
        packageManifestDependencies <- o .: "dependencies"
        pure PackageManifest { .. }

-- >>> eitherDecode testManifest :: Either String PackageManifest
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
