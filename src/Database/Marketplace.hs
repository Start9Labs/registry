{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DeriveGeneric #-}

module Database.Marketplace where

import           Conduit                        ( ConduitT
                                                , MonadResource
                                                , MonadUnliftIO
                                                , awaitForever
                                                , yield
                                                )
import           Database.Esqueleto.Experimental
                                                ( (%)
                                                , (&&.)
                                                , (++.)
                                                , (==.)
                                                , Entity(entityKey, entityVal)
                                                , SqlBackend
                                                , (^.)
                                                , desc
                                                , from
                                                , ilike
                                                , in_
                                                , innerJoin
                                                , on
                                                , orderBy
                                                , select
                                                , selectSource
                                                , val
                                                , valList
                                                , where_
                                                , (||.)
                                                )
import           Database.Esqueleto.Experimental
                                                ( (:&)(..)
                                                , table
                                                )
import           Lib.Types.AppIndex             ( PkgId )
import           Lib.Types.Category
import           Lib.Types.Emver                ( Version
                                                , VersionRange
                                                )
import           Model
import           Startlude               hiding ( (%)
                                                , from
                                                , on
                                                , yield
                                                )

searchServices :: (MonadResource m, MonadIO m)
               => Maybe CategoryTitle
               -> Text
               -> ConduitT () (Entity PkgRecord) (ReaderT SqlBackend m) ()
searchServices Nothing query = selectSource $ do
    service <- from $ table @PkgRecord
    where_
        (   (service ^. PkgRecordDescShort `ilike` (%) ++. val query ++. (%))
        ||. (service ^. PkgRecordDescLong `ilike` (%) ++. val query ++. (%))
        ||. (service ^. PkgRecordTitle `ilike` (%) ++. val query ++. (%))
        )
    orderBy [desc (service ^. PkgRecordUpdatedAt)]
    pure service
searchServices (Just category) query = selectSource $ do
    services <- from
        (do
            (service :& _ :& cat) <-
                from
                $           table @PkgRecord
                `innerJoin` table @PkgCategory
                `on`        (\(s :& sc) -> sc ^. PkgCategoryPkgId ==. s ^. PkgRecordId)
                `innerJoin` table @Category
                `on`        (\(_ :& sc :& cat) -> sc ^. PkgCategoryCategoryId ==. cat ^. CategoryId)
            -- if there is a cateogry, only search in category
            -- weight title, short, long (bitcoin should equal Bitcoin Core)
            where_
                $   cat
                ^.  CategoryName
                ==. val category
                &&. (   (service ^. PkgRecordDescShort `ilike` (%) ++. val query ++. (%))
                    ||. (service ^. PkgRecordDescLong `ilike` (%) ++. val query ++. (%))
                    ||. (service ^. PkgRecordTitle `ilike` (%) ++. val query ++. (%))
                    )
            pure service
        )
    orderBy [desc (services ^. PkgRecordUpdatedAt)]
    pure services

getPkgData :: (MonadResource m, MonadIO m) => [PkgId] -> ConduitT () (Entity PkgRecord) (ReaderT SqlBackend m) ()
getPkgData pkgs = selectSource $ do
    pkgData <- from $ table @PkgRecord
    where_ (pkgData ^. PkgRecordId `in_` valList (PkgRecordKey <$> pkgs))
    pure pkgData

zipVersions :: MonadUnliftIO m
            => ConduitT (Entity PkgRecord) (Entity PkgRecord, [Entity VersionRecord]) (ReaderT SqlBackend m) ()
zipVersions = awaitForever $ \i -> do
    let appDbId = entityKey i
    res <- lift $ select $ do
        v <- from $ table @VersionRecord
        where_ $ v ^. VersionRecordPkgId ==. val appDbId
        pure v
    yield (i, res)

filterOsCompatible :: Monad m
                   => (Version -> Bool)
                   -> ConduitT
                          (Entity PkgRecord, [Entity VersionRecord], VersionRange)
                          (Entity PkgRecord, [Entity VersionRecord], VersionRange)
                          m
                          ()
filterOsCompatible p = awaitForever $ \(app, versions, requestedVersion) -> do
    let compatible = filter (p . versionRecordOsVersion . entityVal) versions
    when (not $ null compatible) $ yield (app, compatible, requestedVersion)
