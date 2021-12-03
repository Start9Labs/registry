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
                                                , (:&)(..)
                                                , (==.)
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
                                                , table
                                                , val
                                                , valList
                                                , where_
                                                , (||.)
                                                )
import qualified Database.Persist              as P
import           Database.Persist.Postgresql
                                         hiding ( (==.)
                                                , getJust
                                                , selectSource
                                                , (||.)
                                                )
import           Handler.Types.Marketplace      ( PackageDependencyMetadata(..)
                                                )
import           Lib.Types.AppIndex             ( PkgId )
import           Lib.Types.Category
import           Lib.Types.Emver                ( Version )
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

getPkgDependencyData :: MonadIO m
                     => Key PkgRecord
                     -> Version
                     -> ReaderT SqlBackend m ([(Entity PkgDependency, Entity PkgRecord)])
getPkgDependencyData pkgId pkgVersion = select $ do
    pd <- from
        (do
            (pkgDepRecord :& depPkgRecord) <-
                from
                $           table @PkgDependency
                `innerJoin` table @PkgRecord
                `on`        (\(pdr :& dpr) -> dpr ^. PkgRecordId ==. pdr ^. PkgDependencyDepId)
            where_ (pkgDepRecord ^. PkgDependencyPkgId ==. (val pkgId))
            where_ (pkgDepRecord ^. PkgDependencyPkgVersion ==. val pkgVersion)
            pure (pkgDepRecord, depPkgRecord)
        )
    pure pd

zipCategories :: MonadUnliftIO m
              => ConduitT
                     (Entity PkgRecord, [Entity VersionRecord])
                     (Entity PkgRecord, [Entity VersionRecord], [Entity Category])
                     (ReaderT SqlBackend m)
                     ()
zipCategories = awaitForever $ \(pkg, vers) -> do
    let pkgDbId = entityKey pkg
    raw <- lift $ select $ do
        (sc :& cat) <-
            from
            $           table @PkgCategory
            `innerJoin` table @Category
            `on`        (\(sc :& cat) -> sc ^. PkgCategoryCategoryId ==. cat ^. CategoryId)
        where_ (sc ^. PkgCategoryPkgId ==. val pkgDbId)
        pure cat
    yield (pkg, vers, raw)

zipVersions :: MonadUnliftIO m
            => ConduitT (Entity PkgRecord) (Entity PkgRecord, [Entity VersionRecord]) (ReaderT SqlBackend m) ()
zipVersions = awaitForever $ \pkg -> do
    let appDbId = entityKey pkg
    res <- lift $ select $ do
        v <- from $ table @VersionRecord
        where_ $ v ^. VersionRecordPkgId ==. val appDbId
        -- first value in list will be latest version
        orderBy [desc (v ^. VersionRecordNumber)]
        pure v
    yield (pkg, res)

zipDependencyVersions :: (Monad m, MonadIO m)
                      => (Entity PkgDependency, Entity PkgRecord)
                      -> ReaderT SqlBackend m PackageDependencyMetadata
zipDependencyVersions (pkgDepRecord, depRecord) = do
    let pkgDbId = entityKey $ depRecord
    depVers <- select $ do
        v <- from $ table @VersionRecord
        where_ $ v ^. VersionRecordPkgId ==. val pkgDbId
        pure v
    pure $ PackageDependencyMetadata { packageDependencyMetadataPkgDependencyRecord = pkgDepRecord
                                     , packageDependencyMetadataDepPkgRecord        = depRecord
                                     , packageDependencyMetadataDepVersions         = depVers
                                     }

fetchAllAppVersions :: MonadUnliftIO m => ConnectionPool -> PkgId -> m [VersionRecord]
fetchAllAppVersions appConnPool appId = do
    entityAppVersions <- runSqlPool (P.selectList [VersionRecordPkgId P.==. PkgRecordKey appId] []) appConnPool
    pure $ entityVal <$> entityAppVersions

fetchLatestApp :: MonadIO m => PkgId -> ReaderT SqlBackend m (Maybe (P.Entity PkgRecord, P.Entity VersionRecord))
fetchLatestApp appId = fmap headMay . sortResults . select $ do
    (service :& version) <-
        from
        $           table @PkgRecord
        `innerJoin` table @VersionRecord
        `on`        (\(service :& version) -> service ^. PkgRecordId ==. version ^. VersionRecordPkgId)
    where_ (service ^. PkgRecordId ==. val (PkgRecordKey appId))
    pure (service, version)
    where sortResults = fmap $ sortOn (Down . versionRecordNumber . entityVal . snd)
