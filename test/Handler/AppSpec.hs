{-# LANGUAGE TypeFamilies #-}

module Handler.AppSpec (spec) where

import Startlude
import Database.Persist.Sql
import Data.Maybe

import TestImport
import Model

spec :: Spec
spec = do
    describe "GET /apps" $
        withApp $ it "returns list of apps" $ do
            request $ do
                setMethod "GET"
                setUrl ("/apps" :: Text)
            bodyContains "bitcoind"
            bodyContains "version: 0.18.1"
            statusIs 200
    describe "GET /apps/:appId with unknown version spec for bitcoin" $
        withApp $ it "fails to get unknown app" $ do
            request $ do
                setMethod "GET"
                setUrl ("/apps/bitcoind.s9pk?spec=0.18.3" :: Text)
            statusIs 404
    describe "GET /apps/:appId with unknown app" $
        withApp $ it "fails to get an unregistered app" $ do
            request $ do
                setMethod "GET"
                setUrl ("/apps/tempapp.s9pk?spec=0.0.1" :: Text)
            statusIs 404
    describe "GET /apps/:appId with existing version spec for bitcoin" $
        withApp $ it "creates app and metric records" $ do
            request $ do
                setMethod "GET"
                setUrl ("/apps/bitcoind.s9pk?spec==0.18.1" :: Text)
            statusIs 200
            apps <- runDBtest $ selectList [SAppAppId ==. "bitcoind"] []
            assertEq "app should exist" (length apps) 1
            let app = fromJust $ head apps
            metrics <- runDBtest $ selectList [MetricAppId ==. entityKey app] []
            assertEq "metric should exist" (length metrics) 1
    describe "GET /apps/:appId with existing version spec for cups" $
        withApp $ it "creates app and metric records" $ do
            request $ do
                setMethod "GET"
                setUrl ("/apps/cups.s9pk?spec=0.2.1" :: Text)
            statusIs 200
            apps <- runDBtest $ selectList [SAppAppId ==. "cups"] []
            assertEq "app should exist" (length apps) 1
            let app = fromJust $ head apps
            metrics <- runDBtest $ selectList [MetricAppId ==. entityKey app] []
            assertEq "metric should exist" (length metrics) 1
            version <- runDBtest $ selectList [SVersionAppId ==. entityKey app] []
            assertEq "version should exist" (length version) 1
    describe "GET /sys/proxy.pac" $
        withApp $ it "does not record metric but request successful" $ do
            request $ do
                setMethod "GET"
                setUrl ("/sys/proxy.pac?spec=0.1.0" :: Text)
            statusIs 200
            -- select * from s_app
            apps <- runDBtest $ selectList ([] :: [Filter SApp])[]
            assertEq "no apps should exist" (length apps) 0
    describe "GET /sys/:sysId" $
        withApp $ it "does not record metric but request successful" $ do
            request $ do
                setMethod "GET"
                setUrl ("/sys/agent?spec=0.0.0" :: Text)
            statusIs 200
            apps <- runDBtest $ selectList ([] :: [Filter SApp])[]
            assertEq "no apps should exist" (length apps) 0
    -- @TODO uncomment when new portable appmgr live
    xdescribe "GET /apps/manifest/#S9PK" $
        withApp $ it "gets bitcoin manifest" $ do
            request $ do
                setMethod "GET"
                setUrl ("/apps/manifest/bitcoind?spec==0.20.1" :: Text)
            statusIs 200
            bodyContains "{\"id\":\"bitcoind\",\"version\":\"0.20.1\",\"title\":\"Bitcoin Core\",\"description\":{\"short\":\"Bitcoin Full Node by Bitcoin Core\",\"long\":\"Bitcoin is an innovative payment network and a new kind of money. Bitcoin uses peer-to-peer technology to operate with no central authority or banks; managing transactions and the issuing of bitcoins is carried out collectively by the network. Bitcoin is open-source; its design is public, nobody owns or controls Bitcoin and everyone can take part. Through many of its unique properties, Bitcoin allows exciting uses that could not be covered by any previous payment system.\"},\"release-notes\":\"https://github.com/bitcoin/bitcoin/blob/master/doc/release-notes/release-notes-0.20.1.md\",\"has-instructions\":true,\"os-version-required\":\">=0.2.4\",\"os-version-recommended\":\">=0.2.4\",\"ports\":[{\"internal\":8332,\"tor\":8332},{\"internal\":8333,\"tor\":8333}],\"image\":{\"type\":\"tar\"},\"mount\":\"/root/.bitcoin\",\"assets\":[{\"src\":\"bitcoin.conf.template\",\"dst\":\".\",\"overwrite\":true}],\"hidden-service-version\":\"v2\",\"dependencies\":{}}"
