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

createApp :: MonadIO m => AppIdentifier -> AppSeed -> ReaderT SqlBackend m (Maybe (Key App))
createApp appId AppSeed{..} = do
    time <- liftIO $ getCurrentTime 
    insertUnique $ App
        time
        Nothing
        title
        appId
        descShort
        descLong
        appVersion
        releaseNotes'
        iconType

createMetric :: MonadIO m => Maybe (Key App) -> AppIdentifier -> ReaderT SqlBackend m ()
createMetric appId event = do
    time <- liftIO $ getCurrentTime 
    insert_ $ Metric
        time
        appId
        event

createAllAppVersions ::  MonadIO m => StoreApp -> AppIdentifier -> ReaderT SqlBackend m ()
createAllAppVersions app appId = do
    time <- liftIO $ getCurrentTime
    -- inseryt new records and replace existing records (matching any unique constraint)
    putMany $ toList $ storeAppToSeed time appId app

storeAppToSeed :: UTCTime -> AppIdentifier -> StoreApp -> NonEmpty App
storeAppToSeed time appId StoreApp{..} = map (\b -> App
        time
        Nothing
        storeAppTitle
        appId
        storeAppDescShort
        storeAppDescLong
        (semver b)
        (releaseNotes b)
        storeAppIconType
    ) storeAppSemver
