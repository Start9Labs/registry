{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Fuse on/on" #-}

module Database.Marketplace where

import           Conduit                        ( ConduitT
                                                , MonadResource
                                                , MonadUnliftIO
                                                , awaitForever
                                                , leftover
                                                , yield
                                                )
import           Control.Monad.Loops            ( unfoldM )
import           Data.Conduit                   ( await )
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
import           Handler.Types.Marketplace      ( PackageDependencyMetadata(..) )
import           Lib.Types.AppIndex             ( PkgId )
import           Lib.Types.Emver                ( Version )
import           Model
import           Startlude               hiding ( (%)
                                                , from
                                                , on
                                                , yield
                                                )

type CategoryTitle = Text

searchServices :: (MonadResource m, MonadIO m)
               => Maybe CategoryTitle
               -> Text
               -> ConduitT () (Entity VersionRecord) (ReaderT SqlBackend m) ()
searchServices Nothing query = selectSource $ do
    service <- from $ table @VersionRecord
    where_
        (   (service ^. VersionRecordDescShort `ilike` (%) ++. val query ++. (%))
        ||. (service ^. VersionRecordDescLong `ilike` (%) ++. val query ++. (%))
        ||. (service ^. VersionRecordTitle `ilike` (%) ++. val query ++. (%))
        )
    orderBy [desc (service ^. VersionRecordUpdatedAt)]
    pure service
searchServices (Just category) query = selectSource $ do
    services <- from
        (do
            (service :& _ :& cat) <-
                from
                $           table @VersionRecord
                `innerJoin` table @PkgCategory
                `on`        (\(s :& sc) -> sc ^. PkgCategoryPkgId ==. s ^. VersionRecordPkgId)
                `innerJoin` table @Category
                `on`        (\(_ :& sc :& cat) -> sc ^. PkgCategoryCategoryId ==. cat ^. CategoryId)
            -- if there is a cateogry, only search in category
            -- weight title, short, long (bitcoin should equal Bitcoin Core)
            where_
                $   cat
                ^.  CategoryName
                ==. val category
                &&. (   (service ^. VersionRecordDescShort `ilike` (%) ++. val query ++. (%))
                    ||. (service ^. VersionRecordDescLong `ilike` (%) ++. val query ++. (%))
                    ||. (service ^. VersionRecordTitle `ilike` (%) ++. val query ++. (%))
                    )
            pure service
        )
    orderBy [desc (services ^. VersionRecordUpdatedAt)]
    pure services

getPkgData :: (MonadResource m, MonadIO m) => [PkgId] -> ConduitT () (Entity VersionRecord) (ReaderT SqlBackend m) ()
getPkgData pkgs = selectSource $ do
    pkgData <- from $ table @VersionRecord
    where_ (pkgData ^. VersionRecordPkgId `in_` valList (PkgRecordKey <$> pkgs))
    pure pkgData

getPkgDependencyData :: MonadIO m
                     => Key PkgRecord
                     -> Version
                     -> ReaderT SqlBackend m [(Entity PkgDependency, Entity PkgRecord)]
getPkgDependencyData pkgId pkgVersion = select $ do
    from
        (do
            (pkgDepRecord :& depPkgRecord) <-
                from
                $           table @PkgDependency
                `innerJoin` table @PkgRecord
                `on`        (\(pdr :& dpr) -> dpr ^. PkgRecordId ==. pdr ^. PkgDependencyDepId)
            where_ (pkgDepRecord ^. PkgDependencyPkgId ==. val pkgId)
            where_ (pkgDepRecord ^. PkgDependencyPkgVersion ==. val pkgVersion)
            pure (pkgDepRecord, depPkgRecord)
        )

zipCategories :: MonadUnliftIO m
              => ConduitT
                     (PkgId, [Entity VersionRecord])
                     (PkgId, [Entity VersionRecord], [Entity Category])
                     (ReaderT SqlBackend m)
                     ()
zipCategories = awaitForever $ \(pkg, vers) -> do
    raw <- lift $ select $ do
        (sc :& cat) <-
            from
            $           table @PkgCategory
            `innerJoin` table @Category
            `on`        (\(sc :& cat) -> sc ^. PkgCategoryCategoryId ==. cat ^. CategoryId)
        where_ (sc ^. PkgCategoryPkgId ==. val (PkgRecordKey pkg))
        pure cat
    yield (pkg, vers, raw)

collateVersions :: MonadUnliftIO m
                => ConduitT (Entity VersionRecord) (PkgId, [Entity VersionRecord]) (ReaderT SqlBackend m) ()
collateVersions = awaitForever $ \v0 -> do
    let pkg = unPkgRecordKey . versionRecordPkgId $ entityVal v0
    let pull = do
            mvn <- await
            case mvn of
                Nothing -> pure Nothing
                Just vn -> do
                    let pkg' = unPkgRecordKey . versionRecordPkgId $ entityVal vn
                    if pkg == pkg' then pure (Just vn) else leftover vn $> Nothing
    ls <- unfoldM pull
    yield (pkg, v0 : ls)

zipDependencyVersions :: (Monad m, MonadIO m)
                      => (Entity PkgDependency, Entity PkgRecord)
                      -> ReaderT SqlBackend m PackageDependencyMetadata
zipDependencyVersions (pkgDepRecord, depRecord) = do
    let pkgDbId = entityKey depRecord
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
