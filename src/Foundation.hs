{-# LANGUAGE InstanceSigs          #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE ViewPatterns          #-}
module Foundation where

import           Startlude

import           Control.Monad.Logger (LogSource)
import           Data.IORef
import           Database.Persist.Sql
import           Lib.Registry
import           Yesod.Core
import           Yesod.Core.Types     (Logger)
import qualified Yesod.Core.Unsafe    as Unsafe
import           Yesod.Persist.Core

import           Settings

-- | The foundation datatype for your application. This can be a good place to
-- keep settings and values requiring initialization before your application
-- starts running, such as database connections. Every handler will have
-- access to the data present here.

data AgentCtx = AgentCtx
    { appSettings          :: AppSettings
    , appConnPool          :: ConnectionPool -- ^ Database connection pool.
    , appLogger            :: Logger
    , appWebServerThreadId :: IORef (Maybe ThreadId)
    }

setWebProcessThreadId :: ThreadId -> AgentCtx -> IO ()
setWebProcessThreadId tid a = writeIORef (appWebServerThreadId a) . Just $ tid

-- This is where we define all of the routes in our application. For a full
-- explanation of the syntax, please see:
-- http://www.yesodweb.com/book/routing-and-handlers
--
-- Note that this is really half the story; in Application.hs, mkYesodDispatch
-- generates the rest of the code. Please see the following documentation
-- for an explanation for this split:
-- http://www.yesodweb.com/book/scaffolding-and-the-site-template#scaffolding-and-the-site-template_foundation_and_application_modules
--
-- This function also generates the following type synonyms:
-- type Handler = HandlerT AgentCtx IO
mkYesodData "AgentCtx" $(parseRoutesFile "config/routes")

-- Please see the documentation for the Yesod typeclass. There are a number
-- of settings which can be configured by overriding methods here.
instance Yesod AgentCtx where

-- Store session data on the client in encrypted cookies,
-- default session idle timeout is 120 minutes
    makeSessionBackend :: AgentCtx -> IO (Maybe SessionBackend)
    makeSessionBackend _ = Just <$> defaultClientSessionBackend
        120    -- timeout in minutes
        "config/client_session_key.aes"

-- Yesod Middleware allows you to run code before and after each handler function.
-- The defaultYesodMiddleware adds the response header "Vary: Accept, Accept-Language" and performs authorization checks.
-- Some users may also want to add the defaultCsrfMiddleware, which:
--   a) Sets a cookie with a CSRF token in it.
--   b) Validates that incoming write requests include that token in either a header or POST parameter.
-- To add it, chain it together with the defaultMiddleware: yesodMiddleware = defaultYesodMiddleware . defaultCsrfMiddleware
-- For details, see the CSRF documentation in the Yesod.Core.Handler module of the yesod-core package.
    yesodMiddleware :: ToTypedContent res => Handler res -> Handler res
    yesodMiddleware = defaultYesodMiddleware

-- What messages should be logged. The following includes all messages when
-- in development, and warnings and errors in production.
    shouldLogIO :: AgentCtx -> LogSource -> LogLevel -> IO Bool
    shouldLogIO app _source level =
        return
            $  appShouldLogAll (appSettings app)
            || level
            == LevelInfo
            || level
            == LevelWarn
            || level
            == LevelError

    makeLogger :: AgentCtx -> IO Logger
    makeLogger = return . appLogger

-- How to run database actions.
instance YesodPersist AgentCtx where
    type YesodPersistBackend AgentCtx = SqlBackend
    runDB :: SqlPersistT Handler a -> Handler a
    runDB action = runSqlPool action . appConnPool =<< getYesod

instance YesodPersistRunner AgentCtx where
    getDBRunner :: Handler (DBRunner AgentCtx, Handler ())
    getDBRunner = defaultGetDBRunner appConnPool

unsafeHandler :: AgentCtx -> Handler a -> IO a
unsafeHandler = Unsafe.fakeHandlerGetLogger appLogger

-- Note: Some functionality previously present in the scaffolding has been
-- moved to documentation in the Wiki. Following are some hopefully helpful
-- links:
--
-- https://github.com/yesodweb/yesod/wiki/Sending-email
-- https://github.com/yesodweb/yesod/wiki/Serve-static-files-from-a-separate-domain
-- https://github.com/yesodweb/yesod/wiki/i18n-messages-in-the-scaffolding

appLogFunc :: AgentCtx -> LogFunc
appLogFunc = appLogger >>= flip messageLoggerSource
