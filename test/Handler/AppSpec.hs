{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Handler.AppSpec
    ( spec
    )
where

import           Startlude
import           Database.Persist.Sql
import           Data.Maybe

import           TestImport
import           Model
import           Handler.Marketplace
import           Seed
import           Lib.Types.AppIndex
import           Data.Aeson
import           Data.Either.Extra
import           Handler.Marketplace            ( PackageListRes )

spec :: Spec
spec = do
    describe "GET /package/index" $ withApp $ it "returns list of packages" $ do
        _ <- seedBitcoinLndStack
        request $ do
            setMethod "GET"
            setUrl ("/package/index" :: Text)
        statusIs 200
        (res :: PackageListRes) <- requireJSONResponse
        assertEq "response should have two packages" (length res) 3
    describe "GET /package/index?ids" $ withApp $ it "returns list of packages at specified version" $ do
        _ <- seedBitcoinLndStack
        request $ do
            setMethod "GET"
            setUrl ("/package/index?ids=[{\"id\":\"bitcoind\",\"version\":\"=0.21.1.2\"}]" :: Text)
        statusIs 200
        (res :: PackageListRes) <- requireJSONResponse
        assertEq "response should have one package" (length res) 1
        let pkg                           = fromJust $ head res
        let (manifest :: PackageManifest) = fromRight' $ eitherDecode $ encode $ packageResManifest pkg
        assertEq "manifest id should be bitcoind" (packageManifestId manifest) "bitcoind"
    describe "GET /package/index?ids"
        $ withApp
        $ it "returns list of packages and dependencies at specified version"
        $ do
              _ <- seedBitcoinLndStack
              request $ do
                  setMethod "GET"
                  setUrl ("/package/index?ids=[{\"id\":\"lnd\",\"version\":\"=0.13.3.1\"}]" :: Text)
              statusIs 200
              (res :: PackageListRes) <- requireJSONResponse
              assertEq "response should have one package" (length res) 1
              let pkg = fromJust $ head res
              assertEq "package dependency metadata should not be empty" (null $ packageResDependencyInfo pkg) False
    describe "GET /package/index?ids" $ withApp $ it "returns list of packages at exactly specified version" $ do
        _ <- seedBitcoinLndStack
        request $ do
            setMethod "GET"
            setUrl ("/package/index?ids=[{\"id\":\"bitcoind\",\"version\":\"=0.21.1.1\"}]" :: Text)
        statusIs 200
        (res :: PackageListRes) <- requireJSONResponse
        assertEq "response should have one package" (length res) 1
        let pkg                           = fromJust $ head res
        let (manifest :: PackageManifest) = fromRight' $ eitherDecode $ encode $ packageResManifest pkg
        assertEq "manifest version should be 0.21.1.1" (packageManifestVersion manifest) "0.21.1.1"
    describe "GET /package/index?ids" $ withApp $ it "returns list of packages at specified version or greater" $ do
        _ <- seedBitcoinLndStack
        request $ do
            setMethod "GET"
            setUrl ("/package/index?ids=[{\"id\":\"bitcoind\",\"version\":\">=0.21.1.1\"}]" :: Text)
        statusIs 200
        (res :: PackageListRes) <- requireJSONResponse
        assertEq "response should have one package" (length res) 1
        let pkg                           = fromJust $ head res
        let (manifest :: PackageManifest) = fromRight' $ eitherDecode $ encode $ packageResManifest pkg
        assertEq "manifest version should be 0.21.1.2" (packageManifestVersion manifest) "0.21.1.2"
    describe "GET /package/index?ids" $ withApp $ it "returns list of packages at specified version or greater" $ do
        _ <- seedBitcoinLndStack
        request $ do
            setMethod "GET"
            setUrl ("/package/index?ids=[{\"id\":\"bitcoind\",\"version\":\">=0.21.1.2\"}]" :: Text)
        statusIs 200
        (res :: PackageListRes) <- requireJSONResponse
        assertEq "response should have one package" (length res) 1
        let pkg                           = fromJust $ head res
        let (manifest :: PackageManifest) = fromRight' $ eitherDecode $ encode $ packageResManifest pkg
        assertEq "manifest version should be 0.21.1.2" (packageManifestVersion manifest) "0.21.1.2"
    describe "GET /package/:pkgId with unknown version spec for bitcoind" $ withApp $ it "fails to get unknown app" $ do
        _ <- seedBitcoinLndStack
        request $ do
            setMethod "GET"
            setUrl ("/package/bitcoind.s9pk?spec==0.20.0" :: Text)
        statusIs 404
    xdescribe "GET /package/:pkgId with unknown package" $ withApp $ it "fails to get an unregistered app" $ do
        _ <- seedBitcoinLndStack
        request $ do
            setMethod "GET"
            setUrl ("/package/tempapp.s9pk?spec=0.0.1" :: Text)
        statusIs 404
    xdescribe "GET /package/:pkgId with package at unknown version"
        $ withApp
        $ it "fails to get an unregistered app"
        $ do
              _ <- seedBitcoinLndStack
              request $ do
                  setMethod "GET"
                  setUrl ("/package/lightning.s9pk?spec==0.0.1" :: Text)
              statusIs 404
    describe "GET /package/:pkgId with existing version spec for bitcoind"
        $ withApp
        $ it "creates app and metric records"
        $ do
              _ <- seedBitcoinLndStack
              request $ do
                  setMethod "GET"
                  setUrl ("/package/bitcoind.s9pk?spec==0.21.1.2" :: Text)
              statusIs 200
              packages <- runDBtest $ selectList [PkgRecordId ==. PkgRecordKey "bitcoind"] []
              assertEq "app should exist" (length packages) 1
              let app = fromJust $ head packages
              metrics <- runDBtest $ selectList [MetricPkgId ==. entityKey app] []
              assertEq "metric should exist" (length metrics) 1
    describe "GET /package/:pkgId with existing version spec for lnd" $ withApp $ it "creates metric records" $ do
        _ <- seedBitcoinLndStack
        request $ do
            setMethod "GET"
            setUrl ("/package/lnd.s9pk?spec=>=0.13.3.0" :: Text)
        statusIs 200
        metrics <- runDBtest $ selectList [MetricPkgId ==. PkgRecordKey "lnd"] []
        assertEq "metric should exist" (length metrics) 1
