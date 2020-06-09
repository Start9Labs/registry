{-# LANGUAGE TypeFamilies #-}

module Handler.AppSpec (spec) where

import Startlude
import TestImport
import Database.Persist.Sql
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
    describe "GET /apps/:appId with existing version spec for bitcoin" $
        withApp $ it "creates app and metric records" $ do
            request $ do
                setMethod "GET"
                setUrl ("/apps/bitcoind.s9pk?spec=0.18.1" :: Text)
            statusIs 200
            apps <- runDBtest $ selectList [SAppAppId ==. "bitcoind"] []
            metrics <- runDBtest $ selectList [MetricEvent ==. "bitcoind"] []
            assertEq "app should exist" (length apps) 1
            assertEq "metric should exist" (length metrics) 1
    describe "GET /apps/:appId with existing version spec for cups" $
        withApp $ it "creates app and metric records" $ do
            request $ do
                setMethod "GET"
                setUrl ("/apps/cups.s9pk?spec=0.2.1" :: Text)
            statusIs 200
            apps <- runDBtest $ selectList [SAppAppId ==. "cups"] []
            metrics <- runDBtest $ selectList [MetricEvent ==. "cups"] []
            assertEq "app should exist" (length apps) 1
            assertEq "metric should exist" (length metrics) 1
