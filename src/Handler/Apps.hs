{-# LANGUAGE QuasiQuotes      #-}
{-# LANGUAGE RecordWildCards  #-}
{-# LANGUAGE TemplateHaskell  #-}
{-# LANGUAGE TypeApplications #-}

module Handler.Apps where

import           Startlude

import           Control.Monad.Logger
import           Data.Aeson
import qualified Data.ByteString.Lazy as BS
import           Data.Conduit
import qualified Data.Conduit.Binary  as CB
import           System.FilePath
import           Yesod.Core

import           Foundation
import           Lib.Resource

pureLog :: Show a => a -> Handler a
pureLog = liftA2 (*>) ($logInfo . show) pure

logRet :: ToJSON a => Handler a -> Handler a
logRet = (>>= liftA2 (*>) ($logInfo . decodeUtf8 . BS.toStrict . encode) pure)

type AppManifestYml = TypedContent
getAppsManifestR :: Handler AppManifestYml
getAppsManifestR = respondSource typePlain $ CB.sourceFile manifestPath .| awaitForever sendChunkBS
