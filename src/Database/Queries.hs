{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}

module Database.Queries where

import Startlude
import Database.Persist.Sql
import Model
import Settings
import Lib.Types.Semver

fetchApp :: MonadIO m => AppIdentifier -> ReaderT SqlBackend m (Maybe (Entity SApp))
fetchApp appId = selectFirst [SAppAppId ==. appId] [] 

fetchAppVersion :: MonadIO m => AppVersion -> Key SApp -> ReaderT SqlBackend m (Maybe (Entity Version))
fetchAppVersion appVersion appId = selectFirst [VersionNumber ==. appVersion, VersionAppId ==. appId] [] 

createApp :: MonadIO m => AppIdentifier -> StoreApp -> ReaderT SqlBackend m (Key SApp)
createApp appId StoreApp{..} = do
    time <- liftIO getCurrentTime
    insert $ SApp
        time
        Nothing
        storeAppTitle
        appId
        storeAppDescShort
        storeAppDescLong
        storeAppIconType
    
createAppVersion :: MonadIO m => Key SApp -> VersionInfo -> ReaderT SqlBackend m (Key Version)
createAppVersion sId VersionInfo{..} = do
    time <- liftIO getCurrentTime
    insert $ Version
        time
        Nothing
        sId
        versionInfoVersion
        versionInfoReleaseNotes

createMetric :: MonadIO m => Key SApp -> Key Version -> ReaderT SqlBackend m ()
createMetric appId versionId = do
    time <- liftIO $ getCurrentTime 
    insert_ $ Metric
        time
        appId
        versionId
