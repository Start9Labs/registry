module Constants where

import           Data.Aeson
import           Data.Aeson.Types
import           Data.Maybe
import           Data.Version          (showVersion)
import           Lib.Types.ServerApp
import           Paths_start9_registry (version)
import           Startlude

configBasePath :: FilePath
configBasePath = "./config"

registryVersion :: AppVersion
registryVersion = fromJust . parseMaybe parseJSON . String . toS . showVersion $ version

getRegistryHostname :: IsString a => a
getRegistryHostname = "registry"
