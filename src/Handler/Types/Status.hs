{-# LANGUAGE NamedFieldPuns #-}
module Handler.Types.Status where

import           Startlude

import           Data.Aeson
import           Data.Text
import           Yesod.Core.Content

import           Lib.Types.Semver
import           Lib.Types.StoreApp

newtype AppVersionRes = AppVersionRes
    { unAppVersionRes :: AppVersion } deriving (Eq, Show)
instance ToJSON AppVersionRes where
    toJSON AppVersionRes{unAppVersionRes} = object ["version" .= unAppVersionRes]
instance ToContent AppVersionRes where
    toContent = toContent . toJSON
instance ToTypedContent AppVersionRes where
    toTypedContent = toTypedContent . toJSON
