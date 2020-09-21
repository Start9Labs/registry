{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}

module Database.Queries where

import           Startlude
import           Database.Persist.Sql
import           Lib.Types.AppIndex
import           Lib.Types.Semver
import           Model

fetchApp :: MonadIO m => AppIdentifier -> ReaderT SqlBackend m (Maybe (Entity SApp))
fetchApp appId = selectFirst [SAppAppId ==. appId] []

fetchAppVersion :: MonadIO m => AppVersion -> Key SApp -> ReaderT SqlBackend m (Maybe (Entity Version))
fetchAppVersion appVersion appId = selectFirst [VersionNumber ==. appVersion, VersionAppId ==. appId] []

createApp :: MonadIO m => AppIdentifier -> StoreApp -> ReaderT SqlBackend m (Maybe (Key SApp))
createApp appId StoreApp {..} = do
    time <- liftIO getCurrentTime
    insertUnique $ SApp time Nothing storeAppTitle appId storeAppDescShort storeAppDescLong storeAppIconType

createAppVersion :: MonadIO m => Key SApp -> VersionInfo -> ReaderT SqlBackend m (Maybe (Key Version))
createAppVersion sId VersionInfo {..} = do
    time <- liftIO getCurrentTime
    insertUnique $ Version time
                           Nothing
                           sId
                           versionInfoVersion
                           versionInfoReleaseNotes
                           versionInfoOsRequired
                           versionInfoOsRecommended

createMetric :: MonadIO m => Key SApp -> Key Version -> ReaderT SqlBackend m ()
createMetric appId versionId = do
    time <- liftIO $ getCurrentTime
    insert_ $ Metric time appId versionId
