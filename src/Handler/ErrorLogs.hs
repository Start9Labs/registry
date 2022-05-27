{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
module Handler.ErrorLogs where

import           Data.Aeson                     ( (.:)
                                                , FromJSON(parseJSON)
                                                , withObject
                                                )
import           Foundation                     ( Handler )
import           Model                          ( EntityField(ErrorLogRecordIncidents)
                                                , ErrorLogRecord(ErrorLogRecord)
                                                )
import           Startlude                      ( ($)
                                                , Applicative(pure)
                                                , Eq
                                                , MonadIO(liftIO)
                                                , Show
                                                , Text
                                                , Word32
                                                , getCurrentTime
                                                , void
                                                )
import           Yesod.Core                     ( requireCheckJsonBody )
import           Yesod.Persist                  ( (+=.)
                                                , runDB
                                                , upsert
                                                )

data ErrorLog = ErrorLog
    { errorLogEpoch      :: !Text
    , errorLogCommitHash :: !Text
    , errorLogSourceFile :: !Text
    , errorLogLine       :: !Word32
    , errorLogTarget     :: !Text
    , errorLogLevel      :: !Text
    , errorLogMessage    :: !Text
    }
    deriving (Eq, Show)

instance FromJSON ErrorLog where
    parseJSON = withObject "Error Log" $ \o -> do
        errorLogEpoch      <- o .: "log-epoch"
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
