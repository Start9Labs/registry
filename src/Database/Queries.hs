{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}

module Database.Queries where

import           Startlude
import           Database.Persist.Sql
import           Lib.Types.AppIndex
import           Lib.Types.Emver
import           Model
import           Orphans.Emver                  ( )

fetchApp :: MonadIO m => AppIdentifier -> ReaderT SqlBackend m (Maybe (Entity SApp))
fetchApp appId = selectFirst [SAppAppId ==. appId] []

fetchAppVersion :: MonadIO m => Version -> Key SApp -> ReaderT SqlBackend m (Maybe (Entity SVersion))
fetchAppVersion appVersion appId = selectFirst [SVersionNumber ==. appVersion, SVersionAppId ==. appId] []

createApp :: MonadIO m => AppIdentifier -> StoreApp -> ReaderT SqlBackend m (Maybe (Key SApp))
createApp appId StoreApp {..} = do
    time <- liftIO getCurrentTime
    insertUnique $ SApp time Nothing storeAppTitle appId storeAppDescShort storeAppDescLong storeAppIconType

createAppVersion :: MonadIO m => Key SApp -> VersionInfo -> ReaderT SqlBackend m (Maybe (Key SVersion))
createAppVersion sId VersionInfo {..} = do
    time <- liftIO getCurrentTime
    insertUnique $ SVersion time
                            Nothing
                            sId
                            versionInfoVersion
                            versionInfoReleaseNotes
                            versionInfoOsRequired
                            versionInfoOsRecommended

createMetric :: MonadIO m => Key SApp -> Key SVersion -> ReaderT SqlBackend m ()
createMetric appId versionId = do
    time <- liftIO $ getCurrentTime
    insert_ $ Metric time appId versionId
