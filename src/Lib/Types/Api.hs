{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE RecordWildCards #-}
module Lib.Types.Api where

import           Startlude

import           Data.Aeson

import           Orphans.Yesod ()

-- data PostWifiRes; TODO: do we need the PostWifiRes or equivalent??
data AddWifiReq = AddWifiReq
  { addWifiSsid :: Text
  , addWifiPass :: Text
  } deriving (Eq, Show)
instance FromJSON AddWifiReq where
    parseJSON = withObject "add wifi req" $ \o -> do
        addWifiSsid <- o .: "ssid"
        addWifiPass <- o .: "password"
        pure AddWifiReq{..}

newtype EnableWifiReq = EnableWifiReq
  { enableWifiSsid :: Text
  } deriving (Eq, Show)
instance FromJSON EnableWifiReq where
    parseJSON = withObject "enable wifi req" $ \o -> do
        enableWifiSsid <- o .: "ssid"
        pure $ EnableWifiReq {..}
