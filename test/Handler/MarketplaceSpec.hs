{-# LANGUAGE TypeFamilies #-}

module Handler.MarketplaceSpec (spec) where

import Startlude hiding (Any)
import Database.Persist.Sql
import Data.Maybe

import TestImport
import Model
import Database.Marketplace
import Lib.Types.Category
import Lib.Types.Emver

spec :: Spec
spec = do
    describe "searchServices with category" $
        withApp $ it "should filter services with featured category" $ do
            time <- liftIO getCurrentTime
            btc <- runDBtest $ insert $ SApp time (Just time) "Bitcoin Core" "bitcoind" "short desc bitcoin" "long desc bitcoin" "png"
            lnd <- runDBtest $ insert $ SApp time (Just time) "Lightning Network Daemon" "lnd" "short desc lnd" "long desc lnd" "png"
            featuredCat <- runDBtest $ insert $ Category time FEATURED Nothing "desc"
            btcCat <- runDBtest $ insert $ Category time BITCOIN Nothing "desc"
            lnCat <- runDBtest $ insert $ Category time LIGHTNING Nothing "desc"
            _ <- runDBtest $ insert_ $ ServiceCategory time btc featuredCat "bitcoin" FEATURED Nothing
            _ <- runDBtest $ insert_ $ ServiceCategory time lnd lnCat "lnd" LIGHTNING Nothing
            _ <- runDBtest $ insert_ $ ServiceCategory time lnd btcCat "lnd" BITCOIN Nothing
            _ <- runDBtest $ insert_ $ ServiceCategory time btc btcCat "bitcon" BITCOIN Nothing
            apps <- runDBtest $ searchServices FEATURED 20 0 ""
            assertEq "should exist" (length apps) 1
            let app' = fromJust $ head apps
            assertEq "should be bitcoin" (sAppTitle $ entityVal app') "Bitcoin Core"
    describe "searchServices with category" $
        withApp $ it "should filter services with bitcoin category" $ do
            time <- liftIO getCurrentTime
            btc <- runDBtest $ insert $ SApp time (Just time) "Bitcoin Core" "bitcoind" "short desc bitcoin" "long desc bitcoin" "png"
            lnd <- runDBtest $ insert $ SApp time (Just time) "Lightning Network Daemon" "lnd" "short desc lnd" "long desc lnd" "png"
            featuredCat <- runDBtest $ insert $ Category time FEATURED Nothing "desc"
            btcCat <- runDBtest $ insert $ Category time BITCOIN Nothing "desc"
            lnCat <- runDBtest $ insert $ Category time LIGHTNING Nothing "desc"
            _ <- runDBtest $ insert_ $ ServiceCategory time btc featuredCat "bitcoin" FEATURED Nothing
            _ <- runDBtest $ insert_ $ ServiceCategory time lnd lnCat "lnd" LIGHTNING Nothing
            _ <- runDBtest $ insert_ $ ServiceCategory time lnd btcCat "lnd" BITCOIN Nothing
            _ <- runDBtest $ insert_ $ ServiceCategory time btc btcCat "bitcon" BITCOIN Nothing
            apps <- runDBtest $ searchServices BITCOIN 20 0 ""
            assertEq "should exist" (length apps) 2
    describe "searchServices with fuzzy query" $
        withApp $ it "runs search service with fuzzy text in long description" $ do
            time <- liftIO getCurrentTime
            app1 <- runDBtest $ insert $ SApp time (Just time) "Bitcoin Core" "bitcoind" "short desc" "long desc" "png"
            app2 <- runDBtest $ insert $ SApp time (Just time) "Lightning Network Daemon" "lnd" "short desc" "lightning long desc" "png"
            cate <- runDBtest $ insert $ Category time FEATURED Nothing "desc"
            _ <- runDBtest $ insert_ $ ServiceCategory time app1 cate "bitcoind" FEATURED Nothing
            _ <- runDBtest $ insert_ $ ServiceCategory time app2 cate "lnd" FEATURED Nothing
            apps <- runDBtest $ searchServices FEATURED 20 0 "lightning"
            assertEq "should exist" (length apps) 1
            let app' = fromJust $ head apps
            print app'
    describe "searchServices with any category" $
        withApp $ it "runs search service for any category" $ do
            time <- liftIO getCurrentTime
            btc <- runDBtest $ insert $ SApp time (Just time) "Bitcoin Core" "bitcoind" "short desc bitcoin" "long desc bitcoin" "png"
            _ <- runDBtest $ insert $ SVersion time (Just time)btc "0.19.0" "notes" Any Any
            _ <- runDBtest $ insert $ SVersion time (Just time)btc "0.20.0" "notes" Any Any
            lnd <- runDBtest $ insert $ SApp time (Just time) "Lightning Network Daemon" "lnd" "short desc lnd" "long desc lnd" "png"
            _ <- runDBtest $ insert $ SVersion time (Just time)lnd "0.18.0" "notes" Any Any
            _ <- runDBtest $ insert $ SVersion time (Just time)lnd "0.17.0" "notes" Any Any
            featuredCat <- runDBtest $ insert $ Category time FEATURED Nothing "desc"
            btcCat <- runDBtest $ insert $ Category time BITCOIN Nothing "desc"
            lnCat <- runDBtest $ insert $ Category time LIGHTNING Nothing "desc"
            _ <- runDBtest $ insert_ $ ServiceCategory time btc featuredCat "bitcoin" FEATURED Nothing
            _ <- runDBtest $ insert_ $ ServiceCategory time lnd lnCat "lnd" LIGHTNING Nothing
            _ <- runDBtest $ insert_ $ ServiceCategory time lnd btcCat "lnd" BITCOIN Nothing
            _ <- runDBtest $ insert_ $ ServiceCategory time btc btcCat "bitcon" BITCOIN Nothing
            apps <- runDBtest $ searchServices ANY 20 0 ""
            assertEq "should exist" (length apps) 2
    -- describe "getServiceVersionsWithReleaseNotes" $
    --     withApp $ it "gets service with mapping of version to release notes" $ do
    --         time <- liftIO getCurrentTime
    --         app <- runDBtest $ insert $ SApp time Nothing "Bitcoin Core" "bitcoin" "short desc" "long desc" "png"
    --         _ <- runDBtest $ insert $ SVersion time Nothing app "0.19.0.0" "release notes 0.19.0.0" "*" "*"
    --         _ <- runDBtest $ insert $ SVersion time Nothing app "0.20.0.0" "release notes 0.19.0.0" "*" "*"
    --         res <- runDBtest $ getServiceVersionsWithReleaseNotes "bitcoin"
    --         print res