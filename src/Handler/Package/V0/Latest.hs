module Handler.Package.V0.Latest where

import Data.Aeson (ToJSON (..), eitherDecode)
import Data.ByteString.Lazy qualified as LBS
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HM
import Data.List (lookup)
import Database.Queries (fetchLatestApp)
import Foundation (Handler)
import Lib.Error (S9Error (..))
import Lib.Types.Core (PkgId)
import Lib.Types.Emver (Version)
import Model (Key (..), VersionRecord (..))
import Network.HTTP.Types (status400)
import Startlude (Either (..), Generic, Maybe (..), Show, catMaybes, encodeUtf8, fst, pure, snd, traverse, ($), (.), (<$>))
import Yesod (Entity (..), ToContent (..), ToTypedContent (..), YesodPersist (runDB), YesodRequest (reqGetParams), getRequest, sendResponseStatus)


newtype VersionLatestRes = VersionLatestRes (HashMap PkgId (Maybe Version))
    deriving (Show, Generic)
instance ToJSON VersionLatestRes
instance ToContent VersionLatestRes where
    toContent = toContent . toJSON
instance ToTypedContent VersionLatestRes where
    toTypedContent = toTypedContent . toJSON


-- TODO refactor with conduit
getVersionLatestR :: Handler VersionLatestRes
getVersionLatestR = do
    getParameters <- reqGetParams <$> getRequest
    case lookup "ids" getParameters of
        Nothing -> sendResponseStatus status400 (InvalidParamsE "get:ids" "<MISSING>")
        Just packages -> case eitherDecode $ LBS.fromStrict $ encodeUtf8 packages of
            Left _ -> sendResponseStatus status400 (InvalidParamsE "get:ids" packages)
            Right p -> do
                let packageList = (,Nothing) <$> p
                found <- runDB $ traverse fetchLatestApp $ fst <$> packageList
                pure $
                    VersionLatestRes $
                        HM.union
                            ( HM.fromList $
                                ( \v ->
                                    (unPkgRecordKey . entityKey $ fst v, Just $ versionRecordNumber $ entityVal $ snd v)
                                )
                                    <$> catMaybes found
                            )
                            $ HM.fromList packageList
