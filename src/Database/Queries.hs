{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}

module Database.Queries where

import           Database.Persist.Sql           ( PersistStoreRead(get)
                                                , PersistStoreWrite(insertKey, insert_, repsert)
                                                , SqlBackend
                                                )
import           Lib.Types.AppIndex             ( PackageManifest(..)
                                                , PkgId
                                                )
import           Lib.Types.Emver                ( Version )
import           Model                          ( Key(PkgRecordKey, VersionRecordKey)
                                                , Metric(Metric)
                                                , PkgRecord(PkgRecord)
                                                , VersionRecord(VersionRecord)
                                                )
import           Orphans.Emver                  ( )
import           Startlude                      ( ($)
                                                , (.)
                                                , ConvertText(toS)
                                                , Maybe(..)
                                                , MonadIO(..)
                                                , ReaderT
                                                , SomeException
                                                , getCurrentTime
                                                , maybe
                                                )
import           System.FilePath                ( takeExtension )
import           UnliftIO                       ( MonadUnliftIO
                                                , try
                                                )

fetchApp :: MonadIO m => PkgId -> ReaderT SqlBackend m (Maybe PkgRecord)
fetchApp = get . PkgRecordKey

fetchAppVersion :: MonadIO m => PkgId -> Version -> ReaderT SqlBackend m (Maybe VersionRecord)
fetchAppVersion pkgId version = get (VersionRecordKey (PkgRecordKey pkgId) version)

createMetric :: MonadIO m => PkgId -> Version -> ReaderT SqlBackend m ()
createMetric appId version = do
    time <- liftIO getCurrentTime
    insert_ $ Metric time (PkgRecordKey appId) version

upsertPackageVersion :: (MonadUnliftIO m) => PackageManifest -> ReaderT SqlBackend m ()
upsertPackageVersion PackageManifest {..} = do
    now <- liftIO getCurrentTime
    let iconType = maybe "png" (toS . takeExtension . toS) packageManifestIcon
    let pkgId    = PkgRecordKey packageManifestId
    let ins = VersionRecord now
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
