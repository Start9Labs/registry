{-# LANGUAGE TypeFamilies #-}

module Handler.AppSpec (spec) where

import Startlude
import TestImport

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
    describe "GET /apps/:appId" $
        withApp $ it "fails to get unknown app" $ do
            request $ do
                setMethod "GET"
                setUrl ("/apps/bitcoind.s9pk?spec=0.18.2" :: Text)
            statusIs 404
    describe "GET /apps/:appId" $
        withApp $ it "makes da records" $ do
            request $ do
                setMethod "GET"
                setUrl ("/apps/bitcoind.s9pk?spec=0.18.1" :: Text)
            statusIs 200