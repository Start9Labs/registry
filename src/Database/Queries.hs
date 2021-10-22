{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}

module Database.Queries where

import           Database.Persist.Sql
import           Lib.Types.AppIndex
import           Lib.Types.Emver
import           Model
import           Orphans.Emver                  ( )
import           Startlude

fetchApp :: MonadIO m => PkgId -> ReaderT SqlBackend m (Maybe (Entity SApp))
fetchApp appId = selectFirst [SAppAppId ==. appId] []

fetchAppVersion :: MonadIO m => Version -> Key SApp -> ReaderT SqlBackend m (Maybe (Entity SVersion))
fetchAppVersion appVersion appId = selectFirst [SVersionNumber ==. appVersion, SVersionAppId ==. appId] []


createMetric :: MonadIO m => Key SApp -> Key SVersion -> ReaderT SqlBackend m ()
createMetric appId versionId = do
    time <- liftIO getCurrentTime
    insert_ $ Metric time appId versionId
