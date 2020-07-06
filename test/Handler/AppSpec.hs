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
                setUrl ("/apps/bitcoind.s9pk?spec=0.18.1" :: Text)
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
            version <- runDBtest $ selectList [VersionAppId ==. entityKey app] []
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