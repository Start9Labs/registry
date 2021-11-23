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
import           Model                          ( EntityField(ErrorLogRecordIncidents)
                                                , ErrorLogRecord(ErrorLogRecord)
                                                )
import           Startlude               hiding ( Handler )
import           Yesod.Core                     ( requireCheckJsonBody )
import           Yesod.Persist                  ( (+=.)
                                                , runDB
                                                , upsert
                                                )

data ErrorLog = ErrorLog
    { errorLogEpoch      :: Word64
    , errorLogCommitHash :: Text
    , errorLogSourceFile :: Text
    , errorLogLine       :: Word32
    , errorLogTarget     :: Text
    , errorLogLevel      :: Text
    , errorLogMessage    :: Text
    }
    deriving (Eq, Show)

instance FromJSON ErrorLog where
    parseJSON = withObject "Error Log" $ \o -> do
        errorLogEpoch <- o .: "log-epoch" >>= withText
            "Word64"
            (\t -> case readMaybe t of
                Nothing -> fail "Invalid Log Epoch"
                Just x  -> pure x
            )
        errorLogCommitHash <- o .: "commit-hash"
        errorLogSourceFile <- o .: "file"
        errorLogLine       <- o .: "line"
        errorLogLevel      <- o .: "level"
        errorLogTarget     <- o .: "target"
        errorLogMessage    <- o .: "log-message"
        pure ErrorLog { .. }


postErrorLogsR :: Handler ()
postErrorLogsR = do
    ErrorLog {..} <- requireCheckJsonBody @_ @ErrorLog
    void $ runDB $ do
        now <- liftIO getCurrentTime
        let logRecord = ErrorLogRecord now
                                       errorLogEpoch
                                       errorLogCommitHash
                                       errorLogSourceFile
                                       errorLogLine
                                       errorLogTarget
                                       errorLogLevel
                                       errorLogMessage
                                       1
        upsert logRecord [ErrorLogRecordIncidents +=. 1]
