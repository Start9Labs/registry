{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE QuasiQuotes      #-}
{-# LANGUAGE RecordWildCards  #-}
{-# LANGUAGE TemplateHaskell  #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleInstances #-}

module Handler.Apps where

import           Startlude

import           Control.Monad.Logger
import           Data.Aeson
import qualified Data.ByteString.Lazy as BS
import           Data.Char
import           Data.Conduit
import qualified Data.Conduit.Binary  as CB
import qualified Data.Text            as T
import           Data.List
import qualified GHC.Show             (Show (..))
import           Network.HTTP.Types
import           System.Directory
import           Yesod.Core
import           Yesod.Persist.Core

import           Foundation
import           Lib.Registry
import           Lib.Semver
import           Lib.Types.Semver
import           System.FilePath      ((<.>), (</>))
import           System.Posix.Files   (fileSize, getFileStatus)
import           Settings
import           Database.Queries
import qualified Data.HashMap.Strict as HM
import           Database.Persist

pureLog :: Show a => a -> Handler a
pureLog = liftA2 (*>) ($logInfo . show) pure

logRet :: ToJSON a => Handler a -> Handler a
logRet = (>>= liftA2 (*>) ($logInfo . decodeUtf8 . BS.toStrict . encode) pure)

data FileExtension = FileExtension FilePath (Maybe String)
instance Show FileExtension where
    show (FileExtension f Nothing)  = f
    show (FileExtension f (Just e)) = f <.> e

getAppsManifestR :: Handler TypedContent
getAppsManifestR = do
    appResourceDir <- (</> "apps" </> "apps.yaml") . resourcesDir . appSettings <$> getYesod 
    respondSource typePlain $ CB.sourceFile appResourceDir .| awaitForever sendChunkBS

getSysR :: Extension "" -> Handler TypedContent
getSysR e = do
    sysResourceDir <- (</> "sys") . resourcesDir . appSettings <$> getYesod
    getApp sysResourceDir e

getAppR :: Extension "s9pk" -> Handler TypedContent
getAppR e = do
    appResourceDir <- (</> "apps") . resourcesDir . appSettings <$> getYesod
    getApp appResourceDir e

getApp :: KnownSymbol a => FilePath -> Extension a -> Handler TypedContent
getApp rootDir ext@(Extension appId) = do
    specString <- T.filter (not . isSpace) . fromMaybe "*" <$> lookupGetParam "spec"
    spec <- case readMaybe specString of
        Nothing -> sendResponseStatus status400 ("Invalid App Version Specification" :: Text)
        Just t  -> pure t
    appVersions <- liftIO $ getAvailableAppVersions rootDir ext
    putStrLn $ "valid appversion for " <> (show ext :: String) <> ": " <> show appVersions
    case getSpecifiedAppVersion spec appVersions of
        Nothing -> notFound
        Just (RegisteredAppVersion (appVersion, filePath)) -> do
            let isApp = isInfixOf "apps" rootDir
            exists <- liftIO $ doesFileExist filePath
            determineEvent exists isApp filePath appVersion
    where
        determineEvent True False fp _ = do
            sz <- liftIO $ fileSize <$> getFileStatus fp
            addHeader "Content-Length" (show sz)
            respondSource typePlain $ CB.sourceFile fp .| awaitForever sendChunkBS
        determineEvent True True  fp av = do
            _ <- recordMetrics appId rootDir av
            sz <- liftIO $ fileSize <$> getFileStatus fp
            addHeader "Content-Length" (show sz)
            respondSource typePlain $ CB.sourceFile fp .| awaitForever sendChunkBS
        determineEvent False _  _ _ = notFound



errOnNothing :: MonadHandler m => Status -> Text -> Maybe a -> m a 
errOnNothing status res entity = case entity of
    Nothing -> sendResponseStatus status res
    Just a -> pure a

recordMetrics :: String -> FilePath -> AppVersion -> HandlerFor AgentCtx ()
recordMetrics appId rootDir appVersion = do
    let appId' = T.pack appId
    manifest <- liftIO $ getAppManifest rootDir
    (storeApp, versionInfo) <- case HM.lookup appId' $ unAppManifest manifest of
                Nothing -> sendResponseStatus status400 ("App not present in manifest" :: Text)
                Just sa -> do
                    -- look up at specfic version
                    vi <- case find ((appVersion ==) . versionInfoVersion) (storeAppVersionInfo sa) of
                            Nothing -> sendResponseStatus status400 ("App version not present in manifest" :: Text)
                            Just x -> pure x
                    pure (sa, vi)
    -- lazy load app at requested version if it does not yet exist to automatically transfer from using apps.yaml
    sa <- runDB $ fetchApp appId'
    (appKey, versionKey) <- case sa of
        Nothing -> do
            appKey' <- runDB $ createApp appId' storeApp >>= errOnNothing status500 "duplicate app created"
            versionKey' <- runDB $ createAppVersion appKey' versionInfo >>= errOnNothing status500 "duplicate app version created"
            pure (appKey', versionKey')
        Just a -> do
            let appKey' = entityKey a
            existingVersion <- runDB $ fetchAppVersion appVersion appKey'
            case existingVersion of
                Nothing -> do
                    appVersion' <- runDB $ createAppVersion appKey' versionInfo >>= errOnNothing status500 "duplicate app version created"
                    pure (appKey', appVersion')
                Just v -> pure (appKey', entityKey v)
    runDB $ createMetric appKey versionKey