{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
-- | Settings are centralized, as much as possible, into this file. This
-- includes database connection settings, static file locations, etc.
-- In addition, you can configure a number of different aspects of Yesod
-- by overriding methods in the Yesod typeclass. That instance is
-- declared in the Foundation.hs file.
module Settings where

import           Startlude

import qualified Control.Exception        as Exception
import           Control.Monad.Fail               (fail)
import           Data.Maybe
import           Data.Aeson
import           Data.Aeson.Types
import           Data.Version          (showVersion)
import           Data.FileEmbed           (embedFile)
import           Data.Yaml                (decodeEither')
import           Database.Persist.Postgresql (PostgresConf)
import           Network.Wai.Handler.Warp (HostPreference)
import           Yesod.Default.Config2    (applyEnvValue, configSettingsYml)
import           Paths_start9_registry (version)
import           Lib.Types.Semver
import           System.FilePath ((</>))
import qualified Data.HashMap.Strict as HM
import Data.Yaml.Config

-- | Runtime settings to configure this application. These settings can be
-- loaded from various sources: defaults, environment variables, config files,
-- theoretically even a database.
data AppSettings = AppSettings
    { appDatabaseConf           :: PostgresConf
    , appHost                   :: HostPreference
    -- ^ Host/interface the server should bind to.
    , appPort                   :: Word16
    -- ^ Port to listen on
    , appIpFromHeader           :: Bool
    -- ^ Get the IP address from the header when logging. Useful when sitting
    -- behind a reverse proxy.

    , appDetailedRequestLogging :: Bool
    -- ^ Use detailed request logging system
    , appShouldLogAll           :: Bool
    -- ^ Should all log messages be displayed?
    , appCompatibilityPath      :: FilePath
    , resourcesDir              :: FilePath
    , sslPath                   :: FilePath
    , registryHostname          :: Text
    , registryVersion           :: AppVersion
    , sslKeyLocation            :: FilePath
    , sslCsrLocation            :: FilePath
    , sslCertLocation           :: FilePath
    }

instance FromJSON AppSettings where
    parseJSON = withObject "AppSettings" $ \o -> do
        appDatabaseConf           <- o .: "database"
        appHost                   <- fromString <$> o .: "host"
        appPort                   <- o .: "port"
        appIpFromHeader           <- o .: "ip-from-header"
        appDetailedRequestLogging <- o .:? "detailed-logging" .!= True
        appShouldLogAll           <- o .:? "should-log-all" .!= False
        appCompatibilityPath      <- o .: "app-compatibility-path"
        resourcesDir              <- o .: "resources-path"
        sslPath                   <- o .: "ssl-path"
        registryHostname          <- o .: "registry-hostname"
        
        let sslKeyLocation = sslPath </> "key.pem"
        let sslCsrLocation = sslPath </> "certificate.csr"
        let sslCertLocation = sslPath </> "certificate.pem"
        let registryVersion = fromJust . parseMaybe parseJSON . String . toS . showVersion $ version

        return AppSettings { .. }

-- | Raw bytes at compile time of @config/settings.yml@
configSettingsYmlBS :: ByteString
configSettingsYmlBS = $(embedFile configSettingsYml)

-- | @config/settings.yml@, parsed to a @Value@.
configSettingsYmlValue :: Value
configSettingsYmlValue =
    either Exception.throw id $ decodeEither' configSettingsYmlBS

-- | A version of @AppSettings@ parsed at compile time from @config/settings.yml@.
compileTimeAppSettings :: AppSettings
compileTimeAppSettings =
    case fromJSON $ applyEnvValue False mempty configSettingsYmlValue of
        Error   e        -> panic $ toS e
        Success settings -> settings

getAppManifest :: FilePath -> IO AppManifest
getAppManifest resourcesDir = do
    let appFile = (</> "apps.yaml") $ resourcesDir
    loadYamlSettings [appFile] [] useEnv

type AppIdentifier = Text
data AppSeed = AppSeed 
    { appSeedTitle :: Text
    , appSeedDescShort :: Text
    , appSeedDescLong :: Text
    , appSeedVersion :: AppVersion
    , appSeedReleaseNotes :: Text
    , appSeedIconType :: Text
    } deriving (Show)
    
data StoreApp = StoreApp 
    { storeAppTitle :: Text
    , storeAppDescShort :: Text
    , storeAppDescLong :: Text
    , storeAppVersionInfo :: NonEmpty VersionInfo
    , storeAppIconType :: Text
    } deriving (Show)

newtype AppManifest = AppManifest { unAppManifest :: HM.HashMap AppIdentifier StoreApp}
    deriving (Show)

instance FromJSON AppManifest where
    parseJSON = withObject "app details to seed" $ \o -> do
        apps <- for (HM.toList o) $ \(appId', c) -> do
            appId <- parseJSON $ String appId'
            config <- parseJSON c
            storeAppTitle <- config .: "title"
            storeAppIconType <- config .: "icon-type"
            storeAppDescShort <- config .: "description" >>= (.: "short")
            storeAppDescLong <- config .: "description" >>= (.: "long")
            storeAppVersionInfo <- config .: "version-info" >>= \case
                [] -> fail "No Valid Version Info"
                (x:xs) -> pure $ x :| xs
            return $ (appId, StoreApp {..})
        return $ AppManifest (HM.fromList apps)

data VersionInfo = VersionInfo 
    { version' :: AppVersion
    , releaseNotes' :: Text
    } deriving (Eq, Ord, Show)

instance FromJSON VersionInfo where
    parseJSON = withObject "version info" $ \o -> do
        version' <- o .: "version"
        releaseNotes' <- o .: "release-notes"
        pure VersionInfo {..}
