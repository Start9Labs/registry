{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}

module Database.Queries where

import           Database.Persist.Sql
import           Lib.Types.AppIndex
import           Lib.Types.Emver
import           Model
import           Orphans.Emver                  ( )
import           Startlude               hiding ( get )

fetchApp :: MonadIO m => PkgId -> ReaderT SqlBackend m (Maybe PkgRecord)
fetchApp = get . PkgRecordKey

fetchAppVersion :: MonadIO m => PkgId -> Version -> ReaderT SqlBackend m (Maybe VersionRecord)
fetchAppVersion pkgId version = get (VersionRecordKey (PkgRecordKey pkgId) version)

createMetric :: MonadIO m => PkgId -> Version -> ReaderT SqlBackend m ()
createMetric appId version = do
    time <- liftIO getCurrentTime
    insert_ $ Metric time (PkgRecordKey appId) (VersionRecordKey (PkgRecordKey appId) version)
