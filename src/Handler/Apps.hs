{-# LANGUAGE TupleSections #-}
{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE QuasiQuotes      #-}
{-# LANGUAGE RecordWildCards  #-}
{-# LANGUAGE TemplateHaskell  #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}

module Handler.Apps where

import           Startlude               hiding ( Handler )

import           Control.Monad.Logger
import           Data.Aeson
import qualified Data.Attoparsec.Text          as Atto
import qualified Data.ByteString.Lazy          as BS
import           Data.Conduit
import qualified Data.Conduit.Binary           as CB
import qualified Data.Text                     as T
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

import           Database.Queries
import           Foundation
import           Lib.External.AppMgr
import           Lib.Registry
import           Lib.Types.AppIndex
import           Lib.Types.Emver
import           Lib.Types.FileSystem
import           Network.Wai                    ( Request(requestHeaderUserAgent) )
import           Settings
import           Util.Shared

pureLog :: Show a => a -> Handler a
pureLog = liftA2 (*>) ($logInfo . show) pure

logRet :: ToJSON a => Handler a -> Handler a
logRet = (>>= liftA2 (*>) ($logInfo . decodeUtf8 . BS.toStrict . encode) pure)

data FileExtension = FileExtension FilePath (Maybe String)
instance Show FileExtension where
    show (FileExtension f Nothing ) = f
    show (FileExtension f (Just e)) = f <.> e

userAgentOsVersionParser :: Atto.Parser Version
userAgentOsVersionParser = do
    void $ (Atto.string "EmbassyOS" <|> Atto.string "AmbassadorOS" <|> Atto.string "MeshOS") *> Atto.char '/'
    parseVersion

getEmbassyOsVersion :: Handler (Maybe Version)
getEmbassyOsVersion = userAgentOsVersion
    where
        userAgentOsVersion =
            (hush . Atto.parseOnly userAgentOsVersionParser . decodeUtf8 <=< requestHeaderUserAgent) <$> waiRequest

getSysR :: Extension "" -> Handler TypedContent
getSysR e = do
    sysResourceDir <- (</> "sys") . resourcesDir . appSettings <$> getYesod
    -- @TODO update with new response type here
    getApp sysResourceDir e

getAppManifestR :: PkgId -> Handler TypedContent
getAppManifestR appId = do
    (appsDir, appMgrDir) <- getsYesod $ ((</> "apps") . resourcesDir &&& staticBinDir) . appSettings
    av                   <- getVersionFromQuery appsDir appExt >>= \case
        Nothing -> sendResponseStatus status404 ("Specified App Version Not Found" :: Text)
        Just v  -> pure v
    let appDir = (<> "/") . (</> show av) . (</> show appId) $ appsDir
    addPackageHeader appMgrDir appDir appExt
    sourceManifest appMgrDir
                   appDir
                   appExt
                   (\bsSource -> respondSource "application/json" (bsSource .| awaitForever sendChunkBS))
    where appExt = Extension (show appId) :: Extension "s9pk"

getAppConfigR :: PkgId -> Handler TypedContent
getAppConfigR appId = do
    appSettings <- appSettings <$> getYesod
    let appsDir   = (</> "apps") . resourcesDir $ appSettings
    let appMgrDir = staticBinDir appSettings
    av <- getVersionFromQuery appsDir appExt >>= \case
        Nothing -> sendResponseStatus status404 ("Specified App Version Not Found" :: Text)
        Just v  -> pure v
    let appDir = (<> "/") . (</> show av) . (</> show appId) $ appsDir
    addPackageHeader appMgrDir appDir appExt
    config <- sourceConfig appMgrDir
                           appDir
                           appExt
                           (\bsSource -> respondSource "application/json" (bsSource .| awaitForever sendChunkBS))
    pure $ TypedContent "application/json" (toContent config)
    where appExt = Extension (show appId) :: Extension "s9pk"

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
    let satisfactory = filter ((<|| spec) . fst . unRegisteredAppVersion) appVersions
    let best = fst . getMaxVersion <$> foldMap (Just . MaxVersion . (, fst . unRegisteredAppVersion)) satisfactory
    (appsDir, appMgrDir) <- getsYesod $ ((</> "apps") . resourcesDir &&& staticBinDir) . appSettings
    case best of
        Nothing -> notFound
        Just (RegisteredAppVersion (appVersion, filePath)) -> do
            exists' <- liftIO $ doesFileExist filePath >>= \case
                True  -> pure Existent
                False -> pure NonExistent
            let appDir = (<> "/") . (</> show appVersion) . (</> toS appId) $ appsDir
            let appExt = Extension (toS appId) :: Extension "s9pk"
            addPackageHeader appMgrDir appDir appExt
            determineEvent exists' (extension ext) filePath appVersion
    where
        determineEvent :: FileExistence -> String -> FilePath -> Version -> HandlerFor RegistryCtx TypedContent
        -- for app files
        determineEvent Existent "s9pk" fp av = do
            _ <- recordMetrics appId av
            chunkIt fp
        -- for png, system, etc
        determineEvent Existent    _ fp _ = chunkIt fp
        determineEvent NonExistent _ _  _ = notFound

chunkIt :: FilePath -> HandlerFor RegistryCtx TypedContent
chunkIt fp = do
    sz <- liftIO $ fileSize <$> getFileStatus fp
    addHeader "Content-Length" (show sz)
    respondSource typeOctet $ CB.sourceFile fp .| awaitForever sendChunkBS

recordMetrics :: String -> Version -> HandlerFor RegistryCtx ()
recordMetrics appId appVersion = do
    let appId' = T.pack appId
    sa <- runDB $ fetchApp $ PkgId appId'
    case sa of
        Nothing -> do
            $logError $ appId' <> " not found in database"
            notFound
        Just a -> do
            let appKey' = entityKey a
            existingVersion <- runDB $ fetchAppVersion appVersion appKey'
            case existingVersion of
                Nothing -> do
                    $logError $ "Version: " <> show appVersion <> " not found in database"
                    notFound
                Just v -> runDB $ createMetric (entityKey a) (entityKey v)

