{-# LANGUAGE TypeFamilies #-}

module Handler.AppSpec
    ( spec
    )
where

import           Startlude
import           Database.Persist.Sql
import           Data.Maybe

import           TestImport
import           Model

spec :: Spec
spec = do
    describe "GET /package/index" $ withApp $ it "returns list of apps" $ do
        request $ do
            setMethod "GET"
            setUrl ("/package/index" :: Text)
        bodyContains "embassy-pages"
        bodyContains "version: 0.1.3"
        statusIs 200
    describe "GET /package/:appId with unknown version spec for embassy-pages"
        $ withApp
        $ it "fails to get unknown app"
        $ do
              request $ do
                  setMethod "GET"
                  setUrl ("/package/embassy-pages.s9pk?spec=0.1.4" :: Text)
              statusIs 404
    describe "GET /package/:appId with unknown app" $ withApp $ it "fails to get an unregistered app" $ do
        request $ do
            setMethod "GET"
            setUrl ("/package/tempapp.s9pk?spec=0.0.1" :: Text)
        statusIs 404
    describe "GET /package/:appId with existing version spec for embassy-pages"
        $ withApp
        $ it "creates app and metric records"
        $ do
              request $ do
                  setMethod "GET"
                  setUrl ("/package/embassy-pages.s9pk?spec==0.1.3" :: Text)
              statusIs 200
              apps <- runDBtest $ selectList [SAppAppId ==. "embassy-pages"] []
              assertEq "app should exist" (length apps) 1
              let app = fromJust $ head apps
              metrics <- runDBtest $ selectList [MetricAppId ==. entityKey app] []
              assertEq "metric should exist" (length metrics) 1
    describe "GET /package/:appId with existing version spec for filebrowser"
        $ withApp
        $ it "creates app and metric records"
        $ do
              request $ do
                  setMethod "GET"
                  setUrl ("/package/filebrowser.s9pk?spec==2.14.1.1" :: Text)
              statusIs 200
              apps <- runDBtest $ selectList [SAppAppId ==. "filebrowser"] []
              assertEq "app should exist" (length apps) 1
              let app = fromJust $ head apps
              metrics <- runDBtest $ selectList [MetricAppId ==. entityKey app] []
              assertEq "metric should exist" (length metrics) 1
              version <- runDBtest $ selectList [SVersionAppId ==. entityKey app] []
              assertEq "version should exist" (length version) 1
    describe "GET /sys/proxy.pac" $ withApp $ it "does not record metric but request successful" $ do
        request $ do
            setMethod "GET"
            setUrl ("/sys/proxy.pac?spec=0.1.0" :: Text)
        statusIs 200
        apps <- runDBtest $ selectList ([] :: [Filter SApp]) []
        assertEq "no apps should exist" (length apps) 0
    describe "GET /sys/:sysId" $ withApp $ it "does not record metric but request successful" $ do
        request $ do
            setMethod "GET"
            setUrl ("/sys/appmgr?spec=0.0.0" :: Text)
        statusIs 200
        apps <- runDBtest $ selectList ([] :: [Filter SApp]) []
        assertEq "no apps should exist" (length apps) 0
