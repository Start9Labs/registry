{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE KindSignatures #-}

module TestImport
    ( module TestImport
    , module X
    ) where

import Startlude
import Application           (makeFoundation, makeLogWare)
import Foundation            as X
import Test.Hspec            as X
import Yesod.Default.Config2 (useEnv, loadYamlSettings)
import Yesod.Test            as X
import Yesod.Core.Unsafe     (fakeHandlerGetLogger)
import Database.Persist.Sql
import Text.Shakespeare.Text (st)
import Yesod.Core
import qualified Data.Text as T

runHandler :: Handler a -> YesodExample AgentCtx a
runHandler handler = do
    app <- getTestYesod
    fakeHandlerGetLogger appLogger app handler

withApp :: SpecWith (TestApp AgentCtx) -> Spec
withApp = before $ do
    settings <- loadYamlSettings
        ["config/settings.yml"]
        []
        useEnv
    foundation <- makeFoundation settings
    wipeDB foundation
    logWare <- liftIO $ makeLogWare foundation
    return (foundation, logWare)

getTables :: DB [Text]
getTables = do
    tables <- rawSql [st|
        SELECT table_name
        FROM information_schema.tables
        WHERE table_schema = 'public'
        AND table_type = 'BASE TABLE';
    |] []

    return $ fmap unSingle tables

wipeDB :: AgentCtx -> IO ()
wipeDB app = runDBWithApp app $ do
    tables <- getTables
    sqlBackend <- ask

    let escapedTables = map (T.unpack . connEscapeName sqlBackend . DBName) tables
        query = "TRUNCATE TABLE " ++ (intercalate ", " escapedTables)
    rawExecute (T.pack query) []

runDBtest :: SqlPersistM a -> YesodExample AgentCtx a
runDBtest query = do
    app <- getTestYesod
    liftIO $ runDBWithApp app query

runDBWithApp :: AgentCtx -> SqlPersistM a -> IO a
runDBWithApp app query = runSqlPersistMPool query (appConnPool app)

-- A convenient synonym for database access functions
type DB a = forall (m :: * -> *).
    (MonadUnliftIO m) => ReaderT SqlBackend m a
