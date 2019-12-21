{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
-- | Settings are centralized, as much as possible, into this file. This
-- includes database connection settings, static file locations, etc.
-- In addition, you can configure a number of different aspects of Yesod
-- by overriding methods in the Yesod typeclass. That instance is
-- declared in the Foundation.hs file.
module Settings where

import           Crypto.Hash
import           Startlude                hiding (hash)

import qualified Control.Exception        as Exception
import           Data.Aeson
import           Data.FileEmbed           (embedFile)
import           Data.Yaml                (decodeEither')
import           Database.Persist.Sqlite  (SqliteConf (..))
import           Network.Wai.Handler.Warp (HostPreference)
import           Yesod.Default.Config2    (applyEnvValue, configSettingsYml)

-- | Runtime settings to configure this application. These settings can be
-- loaded from various sources: defaults, environment variables, config files,
-- theoretically even a database.
data AppSettings = AppSettings
    { appDatabaseConf           :: SqliteConf
    -- ^ Configuration settings for accessing the database.

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
    }

instance FromJSON AppSettings where
    parseJSON = withObject "AppSettings" $ \o -> do
        appDatabaseConf           <-  o .: "database"
        appHost                   <- fromString <$> o .: "host"
        appPort                   <- o .: "port"
        appIpFromHeader           <- o .: "ip-from-header"
        appDetailedRequestLogging <- o .:? "detailed-logging" .!= True
        appShouldLogAll           <- o .:? "should-log-all" .!= False

        return AppSettings { .. }

apNameFromPass :: Text -> Text
apNameFromPass password = prefix <> toS (take 4 hashStr)
  where
    bs      = encodeUtf8 password
    hashed  = hash bs :: Digest SHA256
    hashStr = show hashed :: String
    prefix  = "start9-"

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
