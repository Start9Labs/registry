{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}

module Database.Queries where

import Startlude
import Lib.Types.Semver
import Database.Persist.Sql
import Model
import Settings

fetchApp :: MonadIO m => AppIdentifier -> AppVersion -> ReaderT SqlBackend m (Maybe (Entity App))
fetchApp appId appVersion = selectFirst [AppAppId ==. appId, AppSemver ==. appVersion] [] 

createApp :: MonadIO m => AppIdentifier -> AppSeed -> ReaderT SqlBackend m (Key App)
createApp appId AppSeed{..} = do
    time <- liftIO $ getCurrentTime 
    insert $ App 
        time
        Nothing
        title
        appId
        descShort
        descLong
        semver
        releaseNotes
        iconType

createMetric :: MonadIO m => Maybe (Key App) -> AppIdentifier -> ReaderT SqlBackend m (Key Metric)
createMetric appId event = do
    time <- liftIO $ getCurrentTime 
    insert $ Metric
        time
        appId
        event