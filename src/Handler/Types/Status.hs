{-# LANGUAGE RecordWildCards #-}
module Handler.Types.Status where

import Startlude

import Data.Aeson
import Data.Text
import Yesod.Core.Content

import Lib.Types.ServerApp
import Lib.Types.Semver

data ServerRes = ServerRes
    { serverStatus  :: AppStatus
    , serverVersion :: AppVersion
    , serverSpecs   :: Value
    } deriving (Eq, Show)
instance ToJSON ServerRes where
    toJSON ServerRes{..} = object
        [ "status" .= toUpper (show serverStatus)
        , "versionInstalled" .= serverVersion
        , "specs" .= serverSpecs
        , "versionLatest" .= serverVersion -- TODO: change this.
        ]
instance ToTypedContent ServerRes where
    toTypedContent = toTypedContent . toJSON
instance ToContent ServerRes where
    toContent = toContent . toJSON

newtype AppVersionRes = AppVersionRes
    { unAppVersionRes :: AppVersion } deriving (Eq, Show)
instance ToJSON AppVersionRes where
    toJSON AppVersionRes{unAppVersionRes} = object ["version" .= unAppVersionRes]
instance ToContent AppVersionRes where
    toContent = toContent . toJSON
instance ToTypedContent AppVersionRes where
    toTypedContent = toTypedContent . toJSON
