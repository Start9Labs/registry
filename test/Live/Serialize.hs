{-# LANGUAGE QuasiQuotes #-}

module Live.Serialize where

import           Data.String.Interpolate.IsString

import           Application
import           Lib.External.Registry
import           Startlude

someYaml :: ByteString
someYaml = [i|
bitcoind:
    title: "Bitcoin Core"
    description:
      short: "A Bitcoin Full Node"
      long: "The bitcoin full node implementation by Bitcoin Core."
    version-info:
      - version: 0.18.1
        release-notes: "Some stuff"
    icon-type: png
|]

appRegistryTest :: IO (Either String RegistryRes)
appRegistryTest = flip parseBsManifest someYaml <$> getAppSettings
