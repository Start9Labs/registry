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
import qualified Data.Attoparsec.ByteString.Char8
                                               as Atto
import qualified Data.ByteString.Lazy          as BS
import           Data.Char
import           Data.Conduit
import qualified Data.Conduit.Binary           as CB
import qualified Data.HashMap.Strict           as HM
import qualified Data.Text                     as T
import qualified Data.Yaml                     as Yaml
import           Database.Persist
import qualified GHC.Show                       ( Show(..) )
import           Network.HTTP.Types
import           System.Directory
import           System.FilePath                ( (<.>)
                                                , (</>)
                                                )
import           System.Posix.Files             ( fileSize
                                                , getFileStatus
                                                )
import           Yesod.Core
import           Yesod.Persist.Core

import           Foundation
import           Lib.Registry
import           Lib.Semver
import           Lib.Types.AppIndex
import           Lib.Types.Semver
import           Lib.Types.FileSystem
import           Lib.Error
import           Lib.External.AppMgr
import           Settings
import           Database.Queries
import           Network.Wai                    ( Request(requestHeaderUserAgent) )

pureLog :: Show a => a -> Handler a
pureLog = liftA2 (*>) ($logInfo . show) pure

logRet :: ToJSON a => Handler a -> Handler a
logRet = (>>= liftA2 (*>) ($logInfo . decodeUtf8 . BS.toStrict . encode) pure)

data FileExtension = FileExtension FilePath (Maybe String)
instance Show FileExtension where
    show (FileExtension f Nothing ) = f
    show (FileExtension f (Just e)) = f <.> e

userAgentOsVersionParser :: Atto.Parser AppVersion
userAgentOsVersionParser = do
    void $ (Atto.string "AmbassadorOS" <|> Atto.string "EmbassyOS") *> Atto.char '/'
    semverParserBS

getEmbassyOsVersion :: Handler (Maybe AppVersion)
getEmbassyOsVersion = userAgentOsVersion
    where
        userAgentOsVersion = (hush . Atto.parseOnly userAgentOsVersionParser <=< requestHeaderUserAgent) <$> waiRequest

getAppsManifestR :: Handler TypedContent
getAppsManifestR = do
    osVersion                              <- getEmbassyOsVersion
    appResourceFile                        <- (</> "apps" </> "apps.yaml") . resourcesDir . appSettings <$> getYesod
    manifest@AppManifest { unAppManifest } <- liftIO (Yaml.decodeFileEither appResourceFile) >>= \case
        Left e -> do
            $logError "COULD NOT PARSE APP INDEX! CORRECT IMMEDIATELY!"
            $logError (show e)
            sendResponseStatus status500 ("Internal Server Error" :: Text)
        Right a -> pure a
    let pruned = case osVersion of
            Nothing -> manifest
            Just av -> AppManifest $ HM.mapMaybe (filterOsRecommended av) unAppManifest
    pure $ TypedContent "application/x-yaml" (toContent $ Yaml.encode pruned)

getSysR :: Extension "" -> Handler TypedContent
getSysR e = do
    sysResourceDir <- (</> "sys") . resourcesDir . appSettings <$> getYesod
    getApp sysResourceDir e

getAppManifestR :: AppIdentifier -> Text -> Handler TypedContent
getAppManifestR appId v = do
    appMgrDir <- (<> "/") . staticBinDir . appSettings <$> getYesod
    appDir <- (<> "/") . (</> toS v) . (</> toS appId) . (</> "apps") . resourcesDir . appSettings <$> getYesod
    manifest <- handleS9ErrT $ getManifest appMgrDir appDir appId
    pure $ TypedContent "application/json" (toContent manifest)

getAppConfigR :: AppIdentifier -> Text -> Handler TypedContent
getAppConfigR appId v = do
    appMgrDir <- (<> "/") . staticBinDir . appSettings <$> getYesod
    appDir <- (<> "/") . (</> toS v) . (</> toS appId) . (</> "apps") . resourcesDir . appSettings <$> getYesod
    config <- handleS9ErrT $ getConfig appMgrDir appDir appId
    pure $ TypedContent "application/json" (toContent config)

getAppR :: Extension "s9pk" -> Handler TypedContent
getAppR e = do
    appResourceDir <- (</> "apps") . resourcesDir . appSettings <$> getYesod
    getApp appResourceDir e

getApp :: KnownSymbol a => FilePath -> Extension a -> Handler TypedContent
getApp rootDir ext@(Extension appId) = do
    specString <- T.filter (not . isSpace) . fromMaybe "*" <$> lookupGetParam "spec"
    spec       <- case readMaybe specString of
        Nothing -> sendResponseStatus status400 ("Invalid App Version Specification" :: Text)
        Just t  -> pure t
    appVersions <- liftIO $ getAvailableAppVersions rootDir ext
    putStrLn $ "valid appversion for " <> (show ext :: String) <> ": " <> show appVersions
    case getSpecifiedAppVersion spec appVersions of
        Nothing -> notFound
        Just (RegisteredAppVersion (appVersion, filePath)) -> do
            exists <- liftIO $ doesFileExist filePath >>= \case
                True  -> pure Existent
                False -> pure NonExistent
            determineEvent exists (extension ext) filePath appVersion
    where
        determineEvent :: FileExistence -> String -> FilePath -> AppVersion -> HandlerFor RegistryCtx TypedContent
        -- for app files
        determineEvent Existent "s9pk" fp av = do
            _ <- recordMetrics appId rootDir av
            chunkIt fp
        -- for png, system, etc
        determineEvent Existent    _ fp _ = chunkIt fp
        determineEvent NonExistent _ _  _ = notFound

chunkIt :: FilePath -> HandlerFor RegistryCtx TypedContent
chunkIt fp = do
    sz <- liftIO $ fileSize <$> getFileStatus fp
    addHeader "Content-Length" (show sz)
    respondSource typeOctet $ CB.sourceFile fp .| awaitForever sendChunkBS

recordMetrics :: String -> FilePath -> AppVersion -> HandlerFor RegistryCtx ()
recordMetrics appId rootDir appVersion = do
    let appId' = T.pack appId
    manifest                <- liftIO $ getAppManifest rootDir
    (storeApp, versionInfo) <- case HM.lookup appId' $ unAppManifest manifest of
        Nothing -> sendResponseStatus status400 ("App not present in manifest" :: Text)
        Just sa -> do
            -- look up at specfic version
            vi <- case find ((appVersion ==) . versionInfoVersion) (storeAppVersionInfo sa) of
                Nothing -> sendResponseStatus status400 ("App version not present in manifest" :: Text)
                Just x  -> pure x
            pure (sa, vi)
    -- lazy load app at requested version if it does not yet exist to automatically transfer from using apps.yaml
    sa                   <- runDB $ fetchApp appId'
    (appKey, versionKey) <- case sa of
        Nothing -> do
            appKey'     <- runDB $ createApp appId' storeApp >>= errOnNothing status500 "duplicate app created"
            versionKey' <- runDB $ createAppVersion appKey' versionInfo >>= errOnNothing
                status500
                "duplicate app version created"
            pure (appKey', versionKey')
        Just a -> do
            let appKey' = entityKey a
            existingVersion <- runDB $ fetchAppVersion appVersion appKey'
            case existingVersion of
                Nothing -> do
                    appVersion' <- runDB $ createAppVersion appKey' versionInfo >>= errOnNothing
                        status500
                        "duplicate app version created"
                    pure (appKey', appVersion')
                Just v -> pure (appKey', entityKey v)
    runDB $ createMetric appKey versionKey
