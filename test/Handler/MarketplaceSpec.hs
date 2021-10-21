{-# LANGUAGE TypeFamilies #-}

module Handler.MarketplaceSpec
    ( spec
    ) where

import           Data.Maybe
import           Database.Persist.Sql
import           Startlude               hiding ( Any )

import           Conduit                        ( (.|)
                                                , runConduit
                                                , sinkList
                                                )
import           Database.Marketplace
import           Lib.Types.Category
import           Lib.Types.Emver
import           Model
import           TestImport

spec :: Spec
spec = do
    describe "searchServices with category" $ withApp $ it "should filter services with featured category" $ do
        time <- liftIO getCurrentTime
        btc  <- runDBtest $ insert $ SApp time
                                          (Just time)
                                          "Bitcoin Core"
                                          "bitcoind"
                                          "short desc bitcoin"
                                          "long desc bitcoin"
                                          "png"
        lnd <- runDBtest $ insert $ SApp time
                                         (Just time)
                                         "Lightning Network Daemon"
                                         "lnd"
                                         "short desc lnd"
                                         "long desc lnd"
                                         "png"
        featuredCat <- runDBtest $ insert $ Category time FEATURED Nothing "desc" 0
        btcCat      <- runDBtest $ insert $ Category time BITCOIN Nothing "desc" 0
        lnCat       <- runDBtest $ insert $ Category time LIGHTNING Nothing "desc" 0
        _           <- runDBtest $ insert_ $ ServiceCategory time btc featuredCat "bitcoin" FEATURED Nothing
        _           <- runDBtest $ insert_ $ ServiceCategory time lnd lnCat "lnd" LIGHTNING Nothing
        _           <- runDBtest $ insert_ $ ServiceCategory time lnd btcCat "lnd" BITCOIN Nothing
        _           <- runDBtest $ insert_ $ ServiceCategory time btc btcCat "bitcon" BITCOIN Nothing
        apps        <- runDBtest $ runConduit $ searchServices (Just FEATURED) "" .| sinkList
        assertEq "should exist" (length apps) 1
        let app' = fromJust $ head apps
        assertEq "should be bitcoin" (sAppTitle $ entityVal app') "Bitcoin Core"
    describe "searchServices with category" $ withApp $ it "should filter services with bitcoin category" $ do
        time <- liftIO getCurrentTime
        btc  <- runDBtest $ insert $ SApp time
                                          (Just time)
                                          "Bitcoin Core"
                                          "bitcoind"
                                          "short desc bitcoin"
                                          "long desc bitcoin"
                                          "png"
        lnd <- runDBtest $ insert $ SApp time
                                         (Just time)
                                         "Lightning Network Daemon"
                                         "lnd"
                                         "short desc lnd"
                                         "long desc lnd"
                                         "png"
        featuredCat <- runDBtest $ insert $ Category time FEATURED Nothing "desc" 0
        btcCat      <- runDBtest $ insert $ Category time BITCOIN Nothing "desc" 0
        lnCat       <- runDBtest $ insert $ Category time LIGHTNING Nothing "desc" 0
        _           <- runDBtest $ insert_ $ ServiceCategory time btc featuredCat "bitcoind" FEATURED Nothing
        _           <- runDBtest $ insert_ $ ServiceCategory time lnd lnCat "lnd" LIGHTNING Nothing
        _           <- runDBtest $ insert_ $ ServiceCategory time lnd btcCat "lnd" BITCOIN Nothing
        _           <- runDBtest $ insert_ $ ServiceCategory time btc btcCat "bitcoind" BITCOIN Nothing
        apps        <- runDBtest $ runConduit $ searchServices (Just BITCOIN) "" .| sinkList
        assertEq "should exist" (length apps) 2
    describe "searchServices with fuzzy query"
        $ withApp
        $ it "runs search service with fuzzy text in long description"
        $ do
              time <- liftIO getCurrentTime
              app1 <- runDBtest $ insert $ SApp time
                                                (Just time)
                                                "Bitcoin Core"
                                                "bitcoind"
                                                "short desc"
                                                "long desc"
                                                "png"
              app2 <- runDBtest $ insert $ SApp time
                                                (Just time)
                                                "Lightning Network Daemon"
                                                "lnd"
                                                "short desc"
                                                "lightning long desc"
                                                "png"
              cate <- runDBtest $ insert $ Category time FEATURED Nothing "desc" 0
              _    <- runDBtest $ insert_ $ ServiceCategory time app1 cate "bitcoind" FEATURED Nothing
              _    <- runDBtest $ insert_ $ ServiceCategory time app2 cate "lnd" FEATURED Nothing
              apps <- runDBtest $ runConduit $ searchServices (Just FEATURED) "lightning" .| sinkList
              assertEq "should exist" (length apps) 1
              let app' = fromJust $ head apps
              print app'
    describe "searchServices with any category" $ withApp $ it "runs search service for any category" $ do
        time <- liftIO getCurrentTime
        btc  <- runDBtest $ insert $ SApp time
                                          (Just time)
                                          "Bitcoin Core"
                                          "bitcoind"
                                          "short desc bitcoin"
                                          "long desc bitcoin"
                                          "png"
        print btc
        _   <- runDBtest $ insert $ SVersion time (Just time) btc "0.19.0" "notes" Any Any Nothing
        _   <- runDBtest $ insert $ SVersion time (Just time) btc "0.20.0" "notes" Any Any Nothing
        lnd <- runDBtest $ insert $ SApp time
                                         (Just time)
                                         "Lightning Network Daemon"
                                         "lnd"
                                         "short desc lnd"
                                         "long desc lnd"
                                         "png"
        _           <- runDBtest $ insert $ SVersion time (Just time) lnd "0.18.0" "notes" Any Any Nothing
        _           <- runDBtest $ insert $ SVersion time (Just time) lnd "0.17.0" "notes" Any Any Nothing
        featuredCat <- runDBtest $ insert $ Category time FEATURED Nothing "desc" 0
        btcCat      <- runDBtest $ insert $ Category time BITCOIN Nothing "desc" 0
        lnCat       <- runDBtest $ insert $ Category time LIGHTNING Nothing "desc" 0
        _           <- runDBtest $ insert_ $ ServiceCategory time btc featuredCat "bitcoin" FEATURED Nothing
        _           <- runDBtest $ insert_ $ ServiceCategory time lnd lnCat "lnd" LIGHTNING Nothing
        _           <- runDBtest $ insert_ $ ServiceCategory time lnd btcCat "lnd" BITCOIN Nothing
        _           <- runDBtest $ insert_ $ ServiceCategory time btc btcCat "bitcon" BITCOIN Nothing
        apps        <- runDBtest $ runConduit $ searchServices Nothing "" .| sinkList
        assertEq "should exist" (length apps) 2
