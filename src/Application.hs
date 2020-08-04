{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TupleSections         #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE ViewPatterns          #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Application
    ( appMain
    , makeFoundation
    , makeLogWare
    , shutdownApp
    , shutdownAll
    , shutdownWeb
    , startApp
    , startWeb
    -- * for DevelMain
    , getApplicationRepl
    , getAppSettings
    -- * for GHCI
    , handler
    ) where

import           Startlude

import           Control.Monad.Logger                  (liftLoc, runLoggingT)
import           Data.Aeson
import           Data.Default
import           Data.IORef
import           Database.Persist.Postgresql           (createPostgresqlPool, pgConnStr, pgPoolSize, runSqlPool, runMigration)
import           Language.Haskell.TH.Syntax            (qLocation)
import           Network.Wai
import           Network.Wai.Handler.Warp              (Settings, defaultSettings, defaultShouldDisplayException,
                                                        getPort, setHost, setOnException, setPort)
import           Network.Wai.Handler.WarpTLS
import           Network.Wai.Middleware.AcceptOverride
import           Network.Wai.Middleware.Autohead
import           Network.Wai.Middleware.Cors           (CorsResourcePolicy (..), cors, simpleCorsResourcePolicy)
import           Network.Wai.Middleware.MethodOverride
import           Network.Wai.Middleware.RequestLogger  (Destination (Logger), IPAddrSource (..), OutputFormat (..),
                                                        destination, mkRequestLogger, outputFormat)
import           System.Log.FastLogger                 (defaultBufSize, newStdoutLoggerSet, toLogStr)
import           Yesod.Core
import           Yesod.Core.Types                      hiding (Logger)
import           Yesod.Default.Config2

-- Import all relevant handler modules here.
-- Don't forget to add new modules to your cabal file!
import           Foundation
import           Handler.Apps
import           Handler.Icons
import           Handler.Version
import           Lib.Ssl
import           Settings
import           System.Posix.Process
import           System.Time.Extra
import           Model

-- This line actually creates our YesodDispatch instance. It is the second half
-- of the call to mkYesodData which occurs in Foundation.hs. Please see the
-- comments there for more details.
mkYesodDispatch "RegistryCtx" resourcesRegistryCtx

-- | This function allocates resources (such as a database connection pool),
-- performs initialization and returns a foundation datatype value. This is also
-- the place to put your migrate statements to have automatic database
-- migrations handled by Yesod.
makeFoundation :: AppSettings -> IO RegistryCtx
makeFoundation appSettings = do
    -- Some basic initializations: HTTP connection manager, logger, and static
    -- subsite.
    appLogger <- newStdoutLoggerSet defaultBufSize >>= makeYesodLogger

    appWebServerThreadId <- newIORef Nothing

    appCompatibilityMap <- decode . toS <$> readFile (appCompatibilityPath appSettings) >>= \case
        Nothing -> panic "invalid compatibility config"
        Just x -> pure x

    -- We need a log function to create a connection pool. We need a connection
    -- pool to create our foundation. And we need our foundation to get a
    -- logging function. To get out of this loop, we initially create a
    -- temporary foundation without a real connection pool, get a log function
    -- from there, and then create the real foundation.
    let mkFoundation appConnPool = RegistryCtx {..}
        -- The RegistryCtx {..} syntax is an example of record wild cards. For more
        -- information, see:
        -- https://ocharles.org.uk/blog/posts/2014-12-04-record-wildcards.html
        tempFoundation = mkFoundation $ panic "connPool forced in tempFoundation"
        logFunc = messageLoggerSource tempFoundation appLogger

    -- Create the database connection pool
    pool <- flip runLoggingT logFunc $ createPostgresqlPool
        (pgConnStr $ appDatabaseConf appSettings)
        (pgPoolSize . appDatabaseConf $ appSettings)

    -- Preform database migration using application logging settings
    runLoggingT (runSqlPool (runMigration migrateAll) pool) logFunc

    -- Return the foundation
    return $ mkFoundation pool

-- | Convert our foundation to a WAI Application by calling @toWaiAppPlain@ and
-- applying some additional middlewares.
makeApplication :: RegistryCtx -> IO Application
makeApplication foundation = do
    logWare <- makeLogWare foundation
    let authWare = makeAuthWare foundation
    -- Create the WAI application and apply middlewares
    appPlain <- toWaiAppPlain foundation
    pure . logWare . cors (const . Just $ policy) . authWare . acceptOverride . autohead . methodOverride $ appPlain
    where
        policy = simpleCorsResourcePolicy { corsMethods = ["GET", "HEAD", "OPTIONS", "POST", "PATCH", "PUT", "DELETE"], corsRequestHeaders = ["app-version", "Content-Type", "Authorization"] }

-- TODO: create a middle ware which will attempt to verify an ecdsa signed transaction against one of the public keys
-- in the validDevices table.
-- makeCheckSigWare :: RegistryCtx -> IO Middleware
-- makeCheckSigWare = _

makeLogWare :: RegistryCtx -> IO Middleware
makeLogWare foundation =
    mkRequestLogger def
        { outputFormat =
            if appDetailedRequestLogging $ appSettings foundation
                then Detailed True
                else Apache
                        (if appIpFromHeader $ appSettings foundation
                            then FromFallback
                            else FromSocket)
        , destination = Logger $ loggerSet $ appLogger foundation
        }

-- TODO : what kind of auth is needed here
makeAuthWare :: RegistryCtx -> Middleware
makeAuthWare _ app req res = next
    where
        next :: IO ResponseReceived
        next = app req res

-- | Warp settings for the given foundation value.
warpSettings :: RegistryCtx -> Settings
warpSettings foundation =
      setPort (fromIntegral . appPort $ appSettings foundation)
    $ setHost (appHost $ appSettings foundation)
    $ setOnException (\_req e ->
        when (defaultShouldDisplayException e) $ messageLoggerSource
            foundation
            (appLogger foundation)
            $(qLocation >>= liftLoc)
            "yesod"
            LevelError
            (toLogStr $ "Exception from Warp: " ++ show e))
      defaultSettings

getAppSettings :: IO AppSettings
getAppSettings = loadYamlSettings [configSettingsYml] [] useEnv

-- | The @main@ function for an executable running this site.
appMain :: IO ()
appMain = do
    -- Get the settings from all relevant sources
    settings <- loadYamlSettingsArgs
        -- fall back to compile-time values, set to [] to require values at runtime
        [configSettingsYmlValue]

        -- allow environment variables to override
        useEnv


    -- Generate the foundation from the settings
    makeFoundation settings >>= startApp

startApp :: RegistryCtx -> IO ()
startApp foundation = do
    -- set up ssl certificates
    putStrLn @Text "Setting up SSL"
    _ <- setupSsl $ appSettings foundation
    putStrLn @Text "SSL Setup Complete"

    -- certbot renew loop
    void . forkIO $ forever $ do
        putStrLn $ "DOMAIN: " <> registryHostname (appSettings foundation)
        putStrLn $ "CERT: " <> sslCertLocation (appSettings foundation)
        runReaderT renewSslCerts foundation
        shouldRenew <- doesSslNeedRenew (sslCertLocation $ appSettings foundation)
        when shouldRenew $ do
            putStrLn @Text "Renewing SSL Certs."
            runReaderT renewSslCerts foundation
        sleep 86_400

    startWeb foundation

startWeb :: RegistryCtx -> IO ()
startWeb foundation = do
    app <- makeApplication foundation
    let AppSettings{..} = appSettings foundation
    putStrLn @Text $ "Launching Web Server on port " <> show appPort
    action <- async $ runTLS
        (tlsSettings sslCertLocation sslKeyLocation)
        (warpSettings foundation)
        app

    setWebProcessThreadId (asyncThreadId action) foundation
    wait action

shutdownAll :: [ThreadId] -> IO ()
shutdownAll threadIds = do
    for_ threadIds killThread
    exitImmediately ExitSuccess

-- Careful, you should always spawn this within forkIO so as to avoid accidentally killing the running process
shutdownWeb :: RegistryCtx -> IO ()
shutdownWeb RegistryCtx{..} = do
    mThreadId <- readIORef appWebServerThreadId
    for_ mThreadId $ \tid -> do
        killThread tid
        writeIORef appWebServerThreadId Nothing

--------------------------------------------------------------
-- Functions for DevelMain.hs (a way to run the RegistryCtx from GHCi)
--------------------------------------------------------------

getApplicationRepl :: IO (Int, RegistryCtx, Application)
getApplicationRepl = do
    foundation <- getAppSettings >>= makeFoundation
    wsettings <- getDevSettings $ warpSettings foundation
    app1 <- makeApplication foundation
    return (getPort wsettings, foundation,  app1)

shutdownApp :: RegistryCtx -> IO ()
shutdownApp _ = return ()

---------------------------------------------
-- Functions for use in development with GHCi
---------------------------------------------

-- | Run a handler
handler :: Handler a -> IO a
handler h = getAppSettings >>= makeFoundation >>= flip unsafeHandler h
