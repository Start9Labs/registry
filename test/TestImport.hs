{-# LANGUAGE QuasiQuotes #-}

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
    logWare <- liftIO $ makeLogWare foundation
    return (foundation, logWare)