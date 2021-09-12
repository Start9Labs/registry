{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
module Handler.ErrorLogs where

import           Control.Monad                  ( MonadFail(fail) )
import           Data.Aeson                     ( (.:)
                                                , FromJSON(parseJSON)
                                                , withObject
                                                , withText
                                                )
import           Foundation
import           Settings                       ( AppSettings(errorLogRoot) )
import           Startlude               hiding ( Handler )
import           System.FilePath                ( (<.>)
                                                , (</>)
                                                )
import           Yesod.Core                     ( getsYesod
                                                , requireCheckJsonBody
                                                )

data ErrorLog = ErrorLog
    { errorLogEpoch   :: Word64
    , errorLogMessage :: Text
    }
    deriving (Eq, Show)

-- >>> eitherDecode "{ \"log-epoch\": \"1234\", \"log-message\": \"This is the famous budweiser beer\" }" :: Either String ErrorLog
-- <command line>: /lib/x86_64-linux-gnu/libkrb5.so.26: symbol pthread_cond_destroy version GLIBC_2.3.2 not defined in file libpthread.so.0 with link time reference
instance FromJSON ErrorLog where
    parseJSON = withObject "Error Log" $ \o -> do
        errorLogEpoch <- o .: "log-epoch" >>= withText
            "Word64"
            (\t -> case readMaybe t of
                Nothing -> fail "Invalid Log Epoch"
                Just x  -> pure x
            )
        errorLogMessage <- o .: "log-message"
        pure ErrorLog { .. }


postErrorLogsR :: Handler ()
postErrorLogsR = do
    ErrorLog {..} <- requireCheckJsonBody @_ @ErrorLog
    root          <- getsYesod $ errorLogRoot . appSettings
    void $ liftIO $ forkIO $ appendFile (root </> show errorLogEpoch <.> "log") errorLogMessage
