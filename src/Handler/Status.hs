module Handler.Status where

import           Startlude

import           Constants
import           Foundation
import           Handler.Types.Status
import           Lib.Types.Semver

getVersionR :: AppVersion -> Handler AppVersionRes
getVersionR = pure . AppVersionRes -- $ registryVersion
