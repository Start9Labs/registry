{-# LANGUAGE QuasiQuotes #-}
module Handler.ManifestSpec where

import           Startlude
import           Test.Hspec

import           Data.String.Interpolate.IsString
import           Data.Yaml                        as Yaml

import           Handler.Manifest
import           Lib.Types.Semver

manifest :: ByteString
manifest = [i|cups:
  title: "Cups Messenger"
  description:
    short: "Peer-to-Peer Encrypted Messaging"
    long: "A peer-to-peer encrypted messaging platform that operates over tor.\n\nFor more info: https://start9labs.com/products/cups-messager"
  version-info:
    - version: 0.1.0
      meshos-version-requirement: ">=0.1.0"
      release-notes: |
        # Alpha Release
        - Send messages
        - Recieve messages
        - Contact book
  icon-type: png
bitcoind:
  title: "Bitcoin Core"
  description:
    short: "A Bitcoin Full Node"
    long: "The bitcoin full node implementation by Bitcoin Core.\n\nFor more info: https://start9labs.com/products/bitcoin-core"
  version-info:
    - version: 0.19.0
      release-notes: https://github.com/bitcoin/bitcoin/blob/master/doc/release-notes/release-notes-0.19.0.1.md
    - version: 0.18.1
      release-notes: https://github.com/bitcoin/bitcoin/blob/master/doc/release-notes/release-notes-0.18.1.md
  icon-type: png
uptime:
  title: "Uptime"
  description:
    short: "Easy Server Monitoring"
    long: "Monitor other servers, and send a message if they have a problem.\n\nFor more info: https://start9labs.com/products/uptime"
  version-info:
    - version: 0.1.1
      release-notes: |
        - Longer timeouts
        - Proxy error filtering
    - version: 0.1.0
      release-notes: |
        # Alpha Release
        - Poll servers
        - Call webhooks
  icon-type: png|]

spec :: Spec
spec = do
    let m = Yaml.decodeEither' @AppManifest manifest
    it "parses app manifest" $ m `shouldSatisfy` isRight
    it "compression to zero for app version /= 0.1.0" $ do
        case m of
            Left e -> expectationFailure $ "Manifest parse yielded Left: " <> show e
            Right a -> compressManifest (AppVersion (0,0,0,0)) a `shouldBe` AppManifest mempty
    it "no compression for app version 0.1.0" $ do
        case m of
            Left e -> expectationFailure $ "Manifest parse yielded Left: " <> show e
            Right a -> compressManifest (AppVersion (0,1,0,0)) a `shouldBe` a
