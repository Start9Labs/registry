{-# LANGUAGE TypeFamilies #-}

module Handler.AppSpec (spec) where

import Startlude
import TestImport

spec :: Spec
spec = do
    describe "GET /apps" $ do
        withApp $ it "returns list of apps" $ do
            request $ do
                setMethod "GET"
                setUrl ("/apps" :: Text)
            printBody
            bodyContains ""
            statusIs 200

            

