{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module Handler.Icons where

import Data.Aeson (FromJSON, ToJSON)
import Startlude (Eq, Generic, Read, Show)


data IconType = PNG | JPG | JPEG | SVG
    deriving (Eq, Show, Generic, Read)
instance ToJSON IconType
instance FromJSON IconType
