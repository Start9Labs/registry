{-# LANGUAGE TypeFamilies #-}

module Handler.AppSpec (spec) where

import Startlude
import TestImport

spec :: Spec
spec = describe "GET /apps" $
        withApp $ it "returns list of apps" $ do
            request $ do
                setMethod "GET"
                setUrl ("/apps" :: Text)
            bodyContains "bitcoind"
            bodyContains "version: 0.18.1"
            statusIs 200