{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}

module Database.Queries where

import Startlude
import Lib.Types.Semver
import Database.Persist.Sql
import Model
import Settings

fetchApp :: MonadIO m => AppIdentifier -> AppVersion -> ReaderT SqlBackend m (Maybe (Entity SApp))
fetchApp appId appVersion = selectFirst [SAppAppId ==. appId, SAppVersion ==. appVersion] [] 

createApp :: MonadIO m => AppIdentifier -> AppSeed -> ReaderT SqlBackend m (Maybe (Key SApp))
createApp appId AppSeed{..} = do
    time <- liftIO $ getCurrentTime
    insertUnique $ SApp
        time
        Nothing
        appSeedTitle
        appId
        appSeedDescShort
        appSeedDescLong
        appSeedVersion
        appSeedReleaseNotes
        appSeedIconType

createMetric :: MonadIO m => Maybe (Key SApp) -> AppIdentifier -> ReaderT SqlBackend m ()
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

storeAppToSeed :: UTCTime -> AppIdentifier -> StoreApp -> NonEmpty SApp
storeAppToSeed time appId StoreApp{..} = map (\b -> SApp
        time
        Nothing
        storeAppTitle
        appId
        storeAppDescShort
        storeAppDescLong
        (version' b)
        (releaseNotes' b)
        storeAppIconType
    ) storeAppVersionInfo
