module Handler.Status where

import           Startlude

import           Constants
import           Foundation
import           Handler.Types.Status

getVersionR :: Handler AppVersionRes
getVersionR = pure . AppVersionRes $ registryVersion
