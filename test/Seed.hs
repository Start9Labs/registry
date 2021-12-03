module Seed where

import           Database.Persist.Sql           ( PersistStoreWrite(insert, insertKey, insert_) )
import           Model                          ( Category(Category)
                                                , Key(PkgRecordKey)
                                                , PkgCategory(PkgCategory)
                                                , PkgDependency(PkgDependency)
                                                , PkgRecord(PkgRecord)
                                                , VersionRecord(VersionRecord)
                                                )
import           Startlude                      ( ($)
                                                , Applicative(pure)
                                                , Maybe(Just, Nothing)
                                                , MonadIO(liftIO)
                                                , getCurrentTime
                                                )

import           Lib.Types.Category             ( CategoryTitle(BITCOIN, FEATURED, LIGHTNING) )
import           Prelude                        ( read )
import           TestImport                     ( RegistryCtx
                                                , SIO
                                                , YesodExampleData
                                                , runDBtest
                                                )

seedBitcoinLndStack :: SIO (YesodExampleData RegistryCtx) ()
seedBitcoinLndStack = runDBtest $ do
    time <- liftIO getCurrentTime
    insertKey (PkgRecordKey "bitcoind")
        $ PkgRecord time (Just time) "Bitcoin Core" "short desc bitcoin" "long desc bitcoin" "png"
    _ <- insert $ VersionRecord time (Just time) (PkgRecordKey "bitcoind") "0.21.1.2" "notes" "0.3.0" Nothing
    _ <- insert $ VersionRecord time (Just time) (PkgRecordKey "bitcoind") "0.21.1.1" "notes" "0.3.0" Nothing
    _ <- insertKey (PkgRecordKey "lnd")
        $ PkgRecord time (Just time) "Lightning Network Daemon" "short desc lnd" "long desc lnd" "png"
    _ <- insert $ VersionRecord time (Just time) (PkgRecordKey "lnd") "0.13.3.0" "notes" "0.3.0" Nothing
    _ <- insert $ VersionRecord time (Just time) (PkgRecordKey "lnd") "0.13.3.1" "notes" "0.3.0" Nothing
    _ <- insertKey (PkgRecordKey "btc-rpc-proxy")
        $ PkgRecord time (Just time) "BTC RPC Proxy" "short desc btc-rpc-proxy" "long desc btc-rpc-proxy" "png"
    _ <- insert $ VersionRecord time (Just time) (PkgRecordKey "btc-rpc-proxy") "0.3.2.1" "notes" "0.3.0" Nothing
    featuredCat <- insert $ Category time FEATURED Nothing "desc" 0
    btcCat      <- insert $ Category time BITCOIN Nothing "desc" 0
    lnCat       <- insert $ Category time LIGHTNING Nothing "desc" 0
    _           <- insert_ $ PkgCategory time (PkgRecordKey "bitcoind") featuredCat
    _           <- insert_ $ PkgCategory time (PkgRecordKey "lnd") lnCat
    _           <- insert_ $ PkgCategory time (PkgRecordKey "lnd") btcCat
    _           <- insert_ $ PkgCategory time (PkgRecordKey "bitcoind") btcCat
    _           <- insert_ $ PkgCategory time (PkgRecordKey "btc-rpc-proxy") btcCat
    _           <- insert_
        $ PkgDependency time (PkgRecordKey "lnd") "0.13.3.1" (PkgRecordKey "bitcoind") (read ">=0.21.1.2 <0.22.0")
    _ <- insert_ $ PkgDependency time
                                 (PkgRecordKey "lnd")
                                 "0.13.3.1"
                                 (PkgRecordKey "btc-rpc-proxy")
                                 (read ">=0.3.2.1 <0.4.0")
    pure ()
