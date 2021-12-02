{-# LANGUAGE InstanceSigs          #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE ViewPatterns          #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Foundation where

import           Startlude               hiding ( Handler )

import           Control.Monad.Logger           ( Loc
                                                , LogSource
                                                , LogStr
                                                , ToLogStr(toLogStr)
                                                , fromLogStr
                                                )
import           Database.Persist.Sql    hiding ( update )
import           Lib.Registry
import           Yesod.Core
import           Yesod.Core.Types               ( HandlerData(handlerEnv)
                                                , Logger(loggerDate)
                                                , RunHandlerEnv(rheChild, rheSite)
                                                , loggerPutStr
                                                )
import qualified Yesod.Core.Unsafe             as Unsafe

import           Control.Monad.Logger.Extras    ( wrapSGRCode )
import           Control.Monad.Reader.Has       ( Has(extract, update) )
import           Data.String.Interpolate.IsString
                                                ( i )
import qualified Data.Text                     as T
import           Language.Haskell.TH            ( Loc(..) )
import           Lib.PkgRepository
import           Lib.Types.AppIndex
import           Settings
import           System.Console.ANSI.Codes      ( Color(..)
                                                , ColorIntensity(..)
                                                , ConsoleLayer(Foreground)
                                                , SGR(SetColor)
                                                )
import           System.FilePath                ( (</>) )
import           Yesod.Persist.Core

-- | The foundation datatype for your application. This can be a good place to
-- keep settings and values requiring initialization before your application
-- starts running, such as database connections. Every handler will have
-- access to the data present here.


data RegistryCtx = RegistryCtx
    { appSettings          :: AppSettings
    , appLogger            :: Logger
    , appWebServerThreadId :: MVar (ThreadId, ThreadId)
    , appShouldRestartWeb  :: MVar Bool
    , appConnPool          :: ConnectionPool
    , appStopFsNotify      :: IO Bool
    }
instance Has PkgRepo RegistryCtx where
    extract = do
        liftA2 PkgRepo ((</> "apps") . resourcesDir . appSettings) (staticBinDir . appSettings)
    update f ctx =
        let repo     = f $ extract ctx
            settings = (appSettings ctx) { resourcesDir = pkgRepoFileRoot repo, staticBinDir = pkgRepoAppMgrBin repo }
        in  ctx { appSettings = settings }
instance Has a r => Has a (HandlerData r r) where
    extract = extract . rheSite . handlerEnv
    update f r =
        let ctx = update f (rheSite $ handlerEnv r)
            rhe = (handlerEnv r) { rheSite = ctx, rheChild = ctx }
        in  r { handlerEnv = rhe }
instance Has AppSettings RegistryCtx where
    extract = appSettings
    update f ctx = ctx { appSettings = f (appSettings ctx) }

setWebProcessThreadId :: (ThreadId, ThreadId) -> RegistryCtx -> IO ()
setWebProcessThreadId tid a = putMVar (appWebServerThreadId a) $ tid

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
-- type Handler = HandlerT RegistryCtx IO
mkYesodData "RegistryCtx" $(parseRoutesFile "config/routes")

-- Please see the documentation for the Yesod typeclass. There are a number
-- of settings which can be configured by overriding methods here.
instance Yesod RegistryCtx where

-- Store session data on the client in encrypted cookies,
-- default session idle timeout is 120 minutes
    makeSessionBackend :: RegistryCtx -> IO (Maybe SessionBackend)
    makeSessionBackend _ = pure Nothing

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
    shouldLogIO :: RegistryCtx -> LogSource -> LogLevel -> IO Bool
    shouldLogIO app _source level =
        return $ appShouldLogAll (appSettings app) || level == LevelInfo || level == LevelWarn || level == LevelError

    makeLogger :: RegistryCtx -> IO Logger
    makeLogger = return . appLogger

    messageLoggerSource :: RegistryCtx -> Logger -> Loc -> LogSource -> LogLevel -> LogStr -> IO ()
    messageLoggerSource ctx logger = \loc src lvl str -> do
        shouldLog <- shouldLogIO ctx src lvl
        when shouldLog $ do
            date <- loggerDate logger
            let
                formatted =
                    toLogStr date
                        <> ( toLogStr
                           . wrapSGRCode [SetColor Foreground Vivid (colorFor lvl)]
                           $ fromLogStr
                                 (  " ["
                                 <> renderLvl lvl
                                 <> (if T.null src then mempty else "#" <> toLogStr src)
                                 <> "] "
                                 <> str
                                 )
                           )
                        <> (toLogStr
                               (wrapSGRCode [SetColor Foreground Dull White]
                                            [i| @ #{loc_filename loc}:#{fst $ loc_start loc}\n|]
                               )
                           )
            loggerPutStr logger formatted
        where
            renderLvl lvl = case lvl of
                LevelOther t -> toLogStr t
                _            -> toLogStr @String $ drop 5 $ show lvl
            colorFor = \case
                LevelDebug   -> Green
                LevelInfo    -> Blue
                LevelWarn    -> Yellow
                LevelError   -> Red
                LevelOther _ -> White


-- How to run database actions.
instance YesodPersist RegistryCtx where
    type YesodPersistBackend RegistryCtx = SqlBackend
    runDB :: SqlPersistT Handler a -> Handler a
    runDB action = runSqlPool action . appConnPool =<< getYesod

instance YesodPersistRunner RegistryCtx where
    getDBRunner :: Handler (DBRunner RegistryCtx, Handler ())
    getDBRunner = defaultGetDBRunner appConnPool


unsafeHandler :: RegistryCtx -> Handler a -> IO a
unsafeHandler = Unsafe.fakeHandlerGetLogger appLogger

-- Note: Some functionality previously present in the scaffolding has been
-- moved to documentation in the Wiki. Following are some hopefully helpful
-- links:
--
-- https://github.com/yesodweb/yesod/wiki/Sending-email
-- https://github.com/yesodweb/yesod/wiki/Serve-static-files-from-a-separate-domain
-- https://github.com/yesodweb/yesod/wiki/i18n-messages-in-the-scaffolding

appLogFunc :: RegistryCtx -> LogFunc
appLogFunc = appLogger >>= flip messageLoggerSource
