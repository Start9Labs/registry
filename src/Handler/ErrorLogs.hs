{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
module Handler.ErrorLogs where

import           Control.Monad                  ( MonadFail(fail) )
import           Data.Aeson                     ( (.:)
                                                , FromJSON(parseJSON)
                                                , withObject
                                                , withText
                                                )
import qualified Data.Text                     as T
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
    void $ liftIO $ forkIO $ appendFile (root </> show errorLogEpoch <.> "log") $ if "\n" `T.isSuffixOf` errorLogMessage
        then errorLogMessage
        else T.snoc errorLogMessage '\n'
