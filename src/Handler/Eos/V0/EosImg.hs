{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Handler.Eos.V0.EosImg where

import Data.Attoparsec.Text qualified as Atto
import Data.String.Interpolate.IsString (i)
import Data.Text qualified as T
import Database.Persist (Entity (..))
import Database.Persist.Class (getBy)
import Foundation (Handler, RegistryCtx (..))
import Handler.Util (getVersionSpecFromQuery)
import Lib.Error (S9Error (..))
import Lib.Types.Emver (Version (..), parseVersion, satisfies)
import Model (EosHash (..), Unique (..))
import Network.HTTP.Types (status404)
import Settings (AppSettings (..))
import Startlude (Down (..), Maybe (..), Text, filter, for_, headMay, partitionEithers, pure, show, sortOn, ($), (.), (<$>))
import System.FilePath ((</>))
import UnliftIO.Directory (listDirectory)
import Yesod (Content (..), TypedContent, YesodDB, YesodPersist (runDB), addHeader, getsYesod, respond, sendResponseStatus, typeOctet)
import Yesod.Core (logWarn)
import Data.Maybe (maybe)


getEosR :: Handler TypedContent
getEosR = do
    spec <- getVersionSpecFromQuery
    root <- getsYesod $ (</> "eos") . resourcesDir . appSettings
    subdirs <- listDirectory root
    let (failures, successes) = partitionEithers $ Atto.parseOnly parseVersion . T.pack <$> subdirs
    for_ failures $ \f -> $logWarn [i|Emver Parse Failure for EOS: #{f}|]
    let mVersion = headMay . sortOn Down . filter (`satisfies` spec) $ successes
    case mVersion of
        Nothing -> sendResponseStatus status404 (NotFoundE [i|EOS version satisfying #{spec}|])
        Just version -> do
            let imgPath = root </> show version </> "eos.img"
            h <- runDB $ retrieveHash version
            maybe (pure ()) (addHeader "x-eos-hash") h
            respond typeOctet $ ContentFile imgPath Nothing
    where
        retrieveHash :: Version -> YesodDB RegistryCtx (Maybe Text)
        retrieveHash v = do
            mHash <- getBy (UniqueVersion v)
            case mHash of
                Just h -> pure . Just . eosHashHash . entityVal $ h
                Nothing -> pure Nothing
