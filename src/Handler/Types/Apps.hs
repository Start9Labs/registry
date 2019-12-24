{-# LANGUAGE RecordWildCards #-}
module Handler.Types.Apps where

import           Startlude

import           Data.Aeson
import           Yesod.Core.Content

import           Lib.Types.StoreApp

newtype AvailableAppsRes = AvailableAppsRes
    { availableApps :: [StoreApp]
    } deriving (Eq, Show)
instance ToJSON AvailableAppsRes where
    toJSON = toJSON . availableApps
instance ToTypedContent AvailableAppsRes where
    toTypedContent = toTypedContent . toJSON
instance ToContent AvailableAppsRes where
    toContent = toContent . toJSON
