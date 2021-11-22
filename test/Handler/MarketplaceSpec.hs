{-# LANGUAGE TypeFamilies #-}

module Handler.MarketplaceSpec
    ( spec
    )
where

import           Data.Maybe
import           Database.Persist.Sql
import           Startlude               hiding ( Any )

import           Conduit                        ( (.|)
                                                , runConduit
                                                , sinkList
                                                )
import           Database.Marketplace
import           Lib.Types.Category
import           Model
import           TestImport
import           Seed

spec :: Spec
spec = do
    describe "searchServices with category" $ withApp $ it "should filter services with featured category" $ do
        _        <- seedBitcoinLndStack
        packages <- runDBtest $ runConduit $ searchServices (Just FEATURED) "" .| sinkList
        assertEq "should exist" (length packages) 1
        let pkg = fromJust $ head packages
        assertEq "should be bitcoin" (pkgRecordTitle $ entityVal pkg) "Bitcoin Core"
    describe "searchServices with category" $ withApp $ it "should filter services with bitcoin category" $ do
        _        <- seedBitcoinLndStack
        packages <- runDBtest $ runConduit $ searchServices (Just BITCOIN) "" .| sinkList
        assertEq "should exist" (length packages) 3
    describe "searchServices with fuzzy query"
        $ withApp
        $ it "runs search service with fuzzy text in long description and no category"
        $ do
              _        <- seedBitcoinLndStack
              packages <- runDBtest $ runConduit $ searchServices Nothing "lightning" .| sinkList
              assertEq "should exist" (length packages) 1
              let pkg = fromJust $ head packages
              assertEq "package should be lnd" (entityKey pkg) (PkgRecordKey "lnd")
    describe "searchServices with fuzzy query"
        $ withApp
        $ it "runs search service with fuzzy text in long description and bitcoin category"
        $ do
              _        <- seedBitcoinLndStack
              packages <- runDBtest $ runConduit $ searchServices (Just BITCOIN) "proxy" .| sinkList
              assertEq "should exist" (length packages) 1
              let pkg = fromJust $ head packages
              assertEq "package should be lnc" (entityKey pkg) (PkgRecordKey "btc-rpc-proxy")
    describe "searchServices with any category" $ withApp $ it "runs search service for any category" $ do
        _        <- seedBitcoinLndStack
        packages <- runDBtest $ runConduit $ searchServices Nothing "" .| sinkList
        assertEq "should exist" (length packages) 3
