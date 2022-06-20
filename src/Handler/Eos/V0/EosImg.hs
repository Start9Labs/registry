{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Handler.Eos.V0.EosImg where

import Crypto.Hash (SHA256)
import Crypto.Hash.Conduit (hashFile)
import Data.Attoparsec.Text qualified as Atto
import Data.ByteArray.Encoding (Base (..), convertToBase)
import Data.String.Interpolate.IsString (i)
import Data.Text qualified as T
import Database.Persist (Entity (..), insertUnique)
import Database.Persist.Class (getBy)
import Foundation (Handler, RegistryCtx (..))
import Handler.Util (getVersionSpecFromQuery)
import Lib.Error (S9Error (..))
import Lib.Types.Emver (Version (..), parseVersion, satisfies)
import Model (EosHash (..), Unique (..))
import Network.HTTP.Types (status404)
import Settings (AppSettings (..))
import Startlude (Down (..), FilePath, Maybe (..), Text, decodeUtf8, filter, for_, headMay, partitionEithers, pure, show, sortOn, void, ($), (.), (<$>))
import System.FilePath ((</>))
import UnliftIO.Directory (listDirectory)
import Yesod (Content (..), TypedContent, YesodDB, YesodPersist (runDB), addHeader, getsYesod, respond, sendResponseStatus, typeOctet)
import Yesod.Core (logWarn)


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
            h <- runDB $ retrieveHash version imgPath
            addHeader "x-eos-hash" h
            respond typeOctet $ ContentFile imgPath Nothing
    where
        retrieveHash :: Version -> FilePath -> YesodDB RegistryCtx Text
        retrieveHash v fp = do
            mHash <- getBy (UniqueVersion v)
            case mHash of
                Just h -> pure . eosHashHash . entityVal $ h
                Nothing -> do
                    h <- hashFile @_ @SHA256 fp
                    let t = decodeUtf8 $ convertToBase Base16 h
                    void $ insertUnique (EosHash v t) -- lazily populate
                    pure t
