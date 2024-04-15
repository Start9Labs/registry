{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

module Database.Queries where

import Database.Persist.Sql (
    PersistStoreRead (get),
    PersistStoreWrite (insertKey, insert_, repsertMany, repsert),
    SqlBackend,
 )
import Lib.Types.Core (
    PkgId, OsArch (X86_64, AARCH64),
 )
import Lib.Types.Emver (Version)
import Model (
    Key (PkgRecordKey, VersionRecordKey, VersionPlatformKey),
    Metric (Metric),
    PkgDependency (..),
    PkgRecord (PkgRecord),
    VersionRecord (VersionRecord), VersionPlatform (VersionPlatform), EntityField (VersionPlatformPkgId, VersionPlatformVersionNumber, VersionPlatformArch, AdminPkgsPkgId, AdminPkgsAdmin), PkgRecordId, AdminPkgs, AdminId,
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
    (.), Bool (False), fst, bimap,
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
        VersionRecordUpdatedAt, PkgRecordHidden, VersionPlatformRam
    ),
    Key (unPkgRecordKey),
    PkgCategory,
    VersionRecord (versionRecordNumber, versionRecordPkgId),
 )
import Startlude (
    Applicative (pure),
    Down (Down),
    Eq ((==)),
    Functor (fmap),
    Monad,
    Text,
    headMay,
    snd,
    sortOn,
    ($>),
    (<$>), Int,
 )
import Database.Esqueleto.Experimental (isNothing)
import Database.Esqueleto.Experimental ((<=.))

serviceQuerySource ::
    (MonadResource m, MonadIO m) =>
    Maybe Text ->
    Text ->
    [OsArch] ->
    Maybe Int ->
    ConduitT () (Entity VersionRecord, Entity VersionPlatform) (ReaderT SqlBackend m) ()
serviceQuerySource mCat query arches mRam = selectSource $ do
    (service, vp) <- case mCat of
        Nothing -> do
            (service :& vp :& pr) <- from $ table @VersionRecord 
                `innerJoin` table @VersionPlatform `on` (\(service :& vp) -> (VersionPlatformPkgId === VersionRecordPkgId) (vp :& service))
                `innerJoin` table @PkgRecord `on` (\(v :& _ :& p) -> (PkgRecordId === VersionRecordPkgId) (p :& v))
            where_ (service ^. VersionRecordNumber ==. vp ^. VersionPlatformVersionNumber)
            where_ (vp ^. VersionPlatformArch `in_` (valList arches))
            where_ (vp ^. VersionPlatformRam <=. val mRam ||. isNothing (vp ^. VersionPlatformRam))
            where_ (pr ^. PkgRecordHidden ==. val False)
            where_ $ queryInMetadata query service
            pure (service, vp)
        Just category -> do
            (service :& _ :& cat :& vp :& pr) <-
                from $
                    table @VersionRecord
                        `innerJoin` table @PkgCategory `on` (VersionRecordPkgId === PkgCategoryPkgId)
                        `innerJoin` table @Category `on` (\(_ :& a :& b) -> (PkgCategoryCategoryId === CategoryId) (a :& b))
                        `innerJoin` table @VersionPlatform `on` (\(service :& _ :& _ :& vp) -> (VersionPlatformPkgId === VersionRecordPkgId) (vp :& service))
                        `innerJoin` table @PkgRecord `on` (\(v :& _ :& _ :& _ :& p) -> (PkgRecordId === VersionRecordPkgId) (p :& v))
            -- if there is a cateogry, only search in category
            -- weight title, short, long (bitcoin should equal Bitcoin Core)
            where_ $ cat ^. CategoryName ==. val category &&. queryInMetadata query service
            where_ (service ^. VersionRecordNumber ==. vp ^. VersionPlatformVersionNumber)
            where_ (vp ^. VersionPlatformArch `in_` (valList arches))
            where_ (vp ^. VersionPlatformRam <=. val mRam ||. isNothing (vp ^. VersionPlatformRam))
            where_ (pr ^. PkgRecordHidden ==. val False)
            pure (service, vp)
    orderBy
        [ asc (service ^. VersionRecordPkgId)
        , desc (service ^. VersionRecordNumber)
        , desc (service ^. VersionRecordUpdatedAt)
        ]
    pure (service, vp)

queryInMetadata :: Text -> SqlExpr (Entity VersionRecord) -> (SqlExpr (Value Bool))
queryInMetadata query service =
    (service ^. VersionRecordDescShort `ilike` (%) ++. val query ++. (%))
        ||. (service ^. VersionRecordDescLong `ilike` (%) ++. val query ++. (%))
        ||. (service ^. VersionRecordTitle `ilike` (%) ++. val query ++. (%))


getPkgDataSource :: (MonadResource m, MonadIO m) => [PkgId] -> [OsArch] -> Maybe Int -> ConduitT () (Entity VersionRecord, Entity VersionPlatform) (ReaderT SqlBackend m) ()
getPkgDataSource pkgs arches mRam = selectSource $ do
    (pkgData :& vp) <- from $ table @VersionRecord
        `innerJoin` table @VersionPlatform `on` (\(service :& vp) -> (VersionPlatformPkgId === VersionRecordPkgId) (vp :& service))
    where_ (pkgData ^. VersionRecordNumber ==. vp ^. VersionPlatformVersionNumber)
    where_ (vp ^. VersionPlatformArch `in_` (valList arches))
    where_ (vp ^. VersionPlatformRam <=. val mRam ||. isNothing (vp ^. VersionPlatformRam))
    where_ (pkgData ^. VersionRecordPkgId `in_` valList (PkgRecordKey <$> pkgs))
    pure (pkgData, vp)


getPkgDependencyData ::
    MonadIO m =>
    PkgId ->
    Version ->
    ReaderT SqlBackend m [(P.Entity PkgDependency, P.Entity PkgRecord)]
getPkgDependencyData pkgId pkgVersion = 
    select $
        from $ do
            (pkgDepRecord :& pr) <- from $ table @PkgDependency
                `innerJoin` table @PkgRecord `on` (\(v :& p) -> (PkgRecordId === PkgDependencyPkgId) (p :& v))
            where_ (pkgDepRecord ^. PkgDependencyPkgId ==. val (PkgRecordKey pkgId))
            where_ (pkgDepRecord ^. PkgDependencyPkgVersion ==. val pkgVersion)
            pure (pkgDepRecord, pr)


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
    ConduitT (Entity VersionRecord, Entity VersionPlatform) (PkgId, [(VersionRecord, VersionPlatform)]) (ReaderT SqlBackend m) ()
collateVersions = awaitForever $ \(v0, vp) -> do
    let pkg = unPkgRecordKey . versionRecordPkgId $ entityVal v0
    let pull = do
            mvn <- await
            case mvn of
                Nothing -> pure Nothing
                Just vn -> do
                    let pkg' = unPkgRecordKey . versionRecordPkgId $ entityVal $ fst vn
                    if pkg == pkg' then pure (Just vn) else leftover vn $> Nothing
    ls <- unfoldM pull
    yield (pkg, bimap entityVal entityVal (v0, vp) : fmap (\(v, vp') -> (entityVal v, entityVal vp')) ls)


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


fetchAllPkgVersions :: MonadUnliftIO m => ConnectionPool -> PkgId -> m [VersionRecord]
fetchAllPkgVersions appConnPool appId = do
    entityAppVersions <- runSqlPool (P.selectList [VersionRecordPkgId P.==. PkgRecordKey appId] []) appConnPool
    pure $ entityVal <$> entityAppVersions


fetchAppVersion :: MonadIO m => PkgId -> Version -> ReaderT SqlBackend m (Maybe VersionRecord)
fetchAppVersion pkgId version = get (VersionRecordKey (PkgRecordKey pkgId) version)


fetchLatestApp :: MonadIO m => PkgRecordId -> ReaderT SqlBackend m (Maybe (P.Entity PkgRecord, P.Entity VersionRecord))
fetchLatestApp appId = fmap headMay . sortResults . select $ do
    (service :& version) <-
        from $
            table @PkgRecord
                `innerJoin` table @VersionRecord
                `on` (\(service :& version) -> service ^. PkgRecordId ==. version ^. VersionRecordPkgId)
    where_ (service ^. PkgRecordId ==. val appId)
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
                Nothing
                pkgId
                packageManifestVersion
                packageManifestTitle
                packageManifestDescriptionShort
                packageManifestDescriptionLong
                iconType
                packageManifestReleaseNotes
                packageManifestEosVersion
    _res <- try @_ @SomeException $ insertKey pkgId (PkgRecord False now (Just now))
    repsert (VersionRecordKey pkgId packageManifestVersion) ins

upsertPackageVersionPlatform :: (MonadUnliftIO m) => (Maybe [OsArch]) -> PackageManifest -> ReaderT SqlBackend m ()
upsertPackageVersionPlatform maybeArches PackageManifest{..} = do
    now <- liftIO getCurrentTime
    let pkgId = PkgRecordKey packageManifestId
    let arches = case packageHardwareArch of
            Just a -> a
            Nothing -> case maybeArches of
                Just a -> a
                Nothing -> [X86_64, AARCH64]
    let records = createVersionPlatformRecord now pkgId packageManifestVersion packageHardwareRam packageHardwareDevice <$> arches 
    repsertMany records
    where
        createVersionPlatformRecord time id version ram device arch = ((VersionPlatformKey id version arch), VersionPlatform
            time
            (Just time)
            id
            version
            ram
            device
            arch)

getVersionPlatform ::
    (Monad m, MonadIO m) =>
    PkgRecordId ->
    [OsArch] ->
    ReaderT SqlBackend m [VersionPlatform]
getVersionPlatform pkgId arches = do
    vps <- select $ do
        v <- from $ table @VersionPlatform
        where_ $ v ^. VersionPlatformPkgId ==. val pkgId
        where_ (v ^. VersionPlatformArch `in_` (valList arches))
        pure v
    pure $ entityVal <$> vps

getAllowedPkgs :: (Monad m, MonadIO m) => PkgRecordId -> AdminId -> ReaderT SqlBackend m [AdminPkgs]
getAllowedPkgs pkgId adminId = do
    pkgs <- select $ do
        p <- from $ table @AdminPkgs
        where_ $ p ^. AdminPkgsPkgId ==. val pkgId
        where_ $ p ^. AdminPkgsAdmin ==. val adminId
        pure p
    pure $ entityVal <$> pkgs

getPkg:: (Monad m, MonadIO m) => PkgRecordId -> ReaderT SqlBackend m [PkgRecord]
getPkg pkgId = do
    pkg <- select $ do
        p <- from $ table @PkgRecord
        where_ $ p ^. PkgRecordId ==. val pkgId
        pure p
    pure $ entityVal <$> pkg