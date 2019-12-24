
{-# LANGUAGE RecordWildCards #-}
module Lib.Types.StoreApp where

import           Startlude

import           Control.Monad.Fail
import           Data.Aeson
import           Data.Text

import           Lib.Types.AppsManifest
import           Lib.Types.Semver

data StoreApp = StoreApp
    { storeAppId               :: Text
    , storeAppTitle            :: Text
    , storeAppDescriptionShort :: Text
    , storeAppDescriptionLong  :: Text
    , storeAppIconUrl          :: Text
    , storeAppVersions         :: NonEmpty AppVersionDetails
    } deriving (Eq, Show)

instance ToJSON StoreApp where
    toJSON (StoreApp{..}) = object
                [ "id" .= storeAppId
                , "title" .= storeAppTitle
                , "iconURL" .= storeAppIconUrl
                , "description" .= object
                    [ "short" .= storeAppDescriptionShort
                    , "long" .= storeAppDescriptionLong
                    ]
                , "versionInfo" .= storeAppVersions
                ]
