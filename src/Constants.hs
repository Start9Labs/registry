module Constants where

import           Data.Aeson
import           Data.Aeson.Types
import           Data.Maybe
import           Data.Version          (showVersion)
import           Lib.Types.Semver
import           Paths_start9_registry (version)
import           Startlude

configPath :: FilePath
configPath = "./config"

resourcesPath :: FilePath
resourcesPath = "./resources"

registryVersion :: AppVersion
registryVersion = fromJust . parseMaybe parseJSON . String . toS . showVersion $ version

getRegistryHostname :: IsString a => a
getRegistryHostname = "registry.start9labs.com"
