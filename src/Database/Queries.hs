{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

module Database.Queries where

import Database.Persist.Sql (
    PersistStoreRead (get),
    PersistStoreWrite (insertKey, insert_, repsert),
    SqlBackend,
 )
import Lib.Types.Core (
    PkgId,
 )
import Lib.Types.Emver (Version)
import Model (
    Key (PkgRecordKey, VersionRecordKey),
    Metric (Metric),
    PkgDependency (..),
    PkgRecord (PkgRecord),
    VersionRecord (VersionRecord),
 )
import Orphans.Emver ()
import Startlude (
    ConvertText (toS),
    Maybe (..),
    MonadIO (..),
    ReaderT,
    SomeException,
    getCurrentTime,
    maybe,
    ($),
    (.),
 )
import System.FilePath (takeExtension)
import UnliftIO (
    MonadUnliftIO,
    try,
 )

import Conduit (
    ConduitT,
    MonadResource,
    awaitForever,
    leftover,
    yield,
 )
import Control.Monad.Loops (unfoldM)
import Data.Conduit (await)
import Database.Esqueleto.Experimental (
    PersistEntity,
    SqlExpr,
    Value,
    asc,
    desc,
    from,
    groupBy,
    ilike,
    in_,
    innerJoin,
    on,
    orderBy,
    select,
    selectSource,
    table,
    val,
    valList,
    where_,
    (%),
    (&&.),
    (++.),
    (:&) (..),
    (==.),
    (^.),
    (||.),
 )
import Database.Persist qualified as P
import Database.Persist.Postgresql (
    ConnectionPool,
    Entity (entityVal),
    runSqlPool,
 )
import Lib.Types.Manifest (PackageManifest (..))
import Model (
    Category,
    EntityField (
        CategoryId,
        CategoryName,
        PkgCategoryCategoryId,
        PkgCategoryPkgId,
        PkgDependencyPkgId,
        PkgDependencyPkgVersion,
        PkgRecordId,
        VersionRecordDescLong,
        VersionRecordDescShort,
        VersionRecordNumber,
        VersionRecordPkgId,
        VersionRecordTitle,
        VersionRecordUpdatedAt
    ),
    Key (unPkgRecordKey),
    PkgCategory,
    VersionRecord (versionRecordNumber, versionRecordPkgId),
 )
import Startlude (
    Applicative (pure),
    Bool,
    Down (Down),
    Eq ((==)),
    Functor (fmap),
    Monad,
    Text,
    headMay,
    snd,
    sortOn,
    ($>),
    (<$>),
 )


serviceQuerySource ::
    (MonadResource m, MonadIO m) =>
    Maybe Text ->
    Text ->
    ConduitT () (Entity VersionRecord) (ReaderT SqlBackend m) ()
serviceQuerySource mCat query = selectSource $ do
    service <- case mCat of
        Nothing -> do
            service <- from $ table @VersionRecord
            where_ $ queryInMetadata query service
            pure service
        Just category -> do
            (service :& _ :& cat) <-
                from $
                    table @VersionRecord
                        `innerJoin` table @PkgCategory `on` (VersionRecordPkgId === PkgCategoryPkgId)
                        `innerJoin` table @Category `on` (\(_ :& a :& b) -> (PkgCategoryCategoryId === CategoryId) (a :& b))
            -- if there is a cateogry, only search in category
            -- weight title, short, long (bitcoin should equal Bitcoin Core)
            where_ $ cat ^. CategoryName ==. val category &&. queryInMetadata query service
            pure service
    groupBy (service ^. VersionRecordPkgId, service ^. VersionRecordNumber)
    orderBy
        [ asc (service ^. VersionRecordPkgId)
        , desc (service ^. VersionRecordNumber)
        , desc (service ^. VersionRecordUpdatedAt)
        ]
    pure service


queryInMetadata :: Text -> SqlExpr (Entity VersionRecord) -> (SqlExpr (Value Bool))
queryInMetadata query service =
    (service ^. VersionRecordDescShort `ilike` (%) ++. val query ++. (%))
        ||. (service ^. VersionRecordDescLong `ilike` (%) ++. val query ++. (%))
        ||. (service ^. VersionRecordTitle `ilike` (%) ++. val query ++. (%))


getPkgDataSource :: (MonadResource m, MonadIO m) => [PkgId] -> ConduitT () (Entity VersionRecord) (ReaderT SqlBackend m) ()
getPkgDataSource pkgs = selectSource $ do
    pkgData <- from $ table @VersionRecord
    where_ (pkgData ^. VersionRecordPkgId `in_` valList (PkgRecordKey <$> pkgs))
    pure pkgData


getPkgDependencyData ::
    MonadIO m =>
    PkgId ->
    Version ->
    ReaderT SqlBackend m [PkgDependency]
getPkgDependencyData pkgId pkgVersion = fmap (fmap entityVal) $
    select $
        from $ do
            pkgDepRecord <- from $ table @PkgDependency
            where_ (pkgDepRecord ^. PkgDependencyPkgId ==. val (PkgRecordKey pkgId))
            where_ (pkgDepRecord ^. PkgDependencyPkgVersion ==. val pkgVersion)
            pure pkgDepRecord


(===) ::
    (PersistEntity val1, PersistEntity val2, P.PersistField typ) =>
    EntityField val1 typ ->
    EntityField val2 typ ->
    (SqlExpr (Entity val1) :& SqlExpr (Entity val2)) ->
    SqlExpr (Value Bool)
(===) a' b' (a :& b) = a ^. a' ==. b ^. b'


getCategoriesFor ::
    MonadUnliftIO m =>
    PkgId ->
    ReaderT SqlBackend m [Category]
getCategoriesFor pkg = fmap (fmap entityVal) $
    select $ do
        (sc :& cat) <-
            from $
                table @PkgCategory
                    `innerJoin` table @Category `on` (PkgCategoryCategoryId === CategoryId)
        where_ (sc ^. PkgCategoryPkgId ==. val (PkgRecordKey pkg))
        pure cat


collateVersions ::
    MonadUnliftIO m =>
    ConduitT (Entity VersionRecord) (PkgId, [VersionRecord]) (ReaderT SqlBackend m) ()
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
    yield (pkg, fmap entityVal $ v0 : ls)


getDependencyVersions ::
    (Monad m, MonadIO m) =>
    PkgDependency ->
    ReaderT SqlBackend m [VersionRecord]
getDependencyVersions pkgDepRecord = do
    let pkgDbId = pkgDependencyDepId pkgDepRecord
    depVers <- select $ do
        v <- from $ table @VersionRecord
        where_ $ v ^. VersionRecordPkgId ==. val pkgDbId
        pure v
    pure $ entityVal <$> depVers


fetchAllAppVersions :: MonadUnliftIO m => ConnectionPool -> PkgId -> m [VersionRecord]
fetchAllAppVersions appConnPool appId = do
    entityAppVersions <- runSqlPool (P.selectList [VersionRecordPkgId P.==. PkgRecordKey appId] []) appConnPool
    pure $ entityVal <$> entityAppVersions


fetchAppVersion :: MonadIO m => PkgId -> Version -> ReaderT SqlBackend m (Maybe VersionRecord)
fetchAppVersion pkgId version = get (VersionRecordKey (PkgRecordKey pkgId) version)


fetchLatestApp :: MonadIO m => PkgId -> ReaderT SqlBackend m (Maybe (P.Entity PkgRecord, P.Entity VersionRecord))
fetchLatestApp appId = fmap headMay . sortResults . select $ do
    (service :& version) <-
        from $
            table @PkgRecord
                `innerJoin` table @VersionRecord
                `on` (\(service :& version) -> service ^. PkgRecordId ==. version ^. VersionRecordPkgId)
    where_ (service ^. PkgRecordId ==. val (PkgRecordKey appId))
    pure (service, version)
    where
        sortResults = fmap $ sortOn (Down . versionRecordNumber . entityVal . snd)


createMetric :: MonadIO m => PkgId -> Version -> ReaderT SqlBackend m ()
createMetric appId version = do
    time <- liftIO getCurrentTime
    insert_ $ Metric time (PkgRecordKey appId) version


upsertPackageVersion :: (MonadUnliftIO m) => PackageManifest -> ReaderT SqlBackend m ()
upsertPackageVersion PackageManifest{..} = do
    now <- liftIO getCurrentTime
    let iconType = maybe "png" (toS . takeExtension . toS) packageManifestIcon
    let pkgId = PkgRecordKey packageManifestId
    let ins =
            VersionRecord
                now
                (Just now)
                pkgId
                packageManifestVersion
                packageManifestTitle
                packageManifestDescriptionShort
                packageManifestDescriptionLong
                iconType
                packageManifestReleaseNotes
                packageManifestEosVersion
                Nothing
    _res <- try @_ @SomeException $ insertKey pkgId (PkgRecord now (Just now))
    repsert (VersionRecordKey pkgId packageManifestVersion) ins
