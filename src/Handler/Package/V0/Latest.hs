module Handler.Package.V0.Latest where

import Conduit (concatMapC, mapC, runConduit, sinkList, (.|))
import Data.Aeson (ToJSON (..), eitherDecode)
import Data.ByteString.Lazy qualified as LBS
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HM
import Data.List (lookup)
import Data.List.NonEmpty.Extra qualified as NE
import Data.Tuple.Extra (second)
import Database.Queries (collateVersions, getPkgDataSource)
import Foundation (Handler, RegistryCtx (appSettings))
import Handler.Package.V1.Index (getOsVersionCompat)
import Lib.Error (S9Error (..))
import Lib.Types.Core (PkgId)
import Lib.Types.Emver (Version (..), satisfies)
import Model (VersionRecord (..))
import Network.HTTP.Types (status400)
import Startlude (Bool (True), Down (Down), Either (..), Generic, Maybe (..), NonEmpty, Show, const, encodeUtf8, filter, flip, nonEmpty, pure, ($), (.), (<$>), (<&>), (>>=))
import Yesod (ToContent (..), ToTypedContent (..), YesodPersist (runDB), YesodRequest (reqGetParams), getRequest, sendResponseStatus)
import Handler.Util (filterDeprecatedVersions, getPkgArch)
import Yesod.Core (getsYesod)
import Settings (AppSettings(communityVersion))


newtype VersionLatestRes = VersionLatestRes (HashMap PkgId (Maybe Version))
    deriving (Show, Generic)
instance ToJSON VersionLatestRes
instance ToContent VersionLatestRes where
    toContent = toContent . toJSON
instance ToTypedContent VersionLatestRes where
    toTypedContent = toTypedContent . toJSON


getVersionLatestR :: Handler VersionLatestRes
getVersionLatestR = do
    getParameters <- reqGetParams <$> getRequest
    osPredicate' <-
        getOsVersionCompat <&> \case
            Nothing -> const True
            Just v -> flip satisfies v
    pkgArch <- getPkgArch >>= \case
        Nothing -> pure []
        Just a -> pure a
    communityServiceDeprecationVersion <- getsYesod $ communityVersion . appSettings
    do
        case lookup "ids" getParameters of
            Nothing -> sendResponseStatus status400 (InvalidParamsE "get:ids" "<MISSING>")
            Just packages -> case eitherDecode $ LBS.fromStrict $ encodeUtf8 packages of
                Left _ -> sendResponseStatus status400 (InvalidParamsE "get:ids" packages)
                Right p -> do
                    let packageList = (,Nothing) <$> p
                    let source = getPkgDataSource p pkgArch
                    filteredPackages <-
                        runDB $
                            runConduit $
                                source
                                    -- group conduit pipeline by pkg id
                                    .| collateVersions
                                    -- filter out versions of apps that are incompatible with the OS predicate
                                    .| mapC (second (filter (osPredicate' . versionRecordOsVersion)))
                                    -- filter out deprecated service versions after community registry release
                                    .| mapC (second (filterDeprecatedVersions communityServiceDeprecationVersion osPredicate'))
                                    -- prune empty version sets
                                    .| concatMapC (\(pkgId, vs) -> (pkgId,) <$> nonEmpty vs)
                                    -- grab the latest matching version if it exists
                                    .| mapC (\(a, b) -> (a, (Just $ selectLatestVersion b)))
                                    .| sinkList
                    -- if the requested package does not have available versions, return it as a key with a null value
                    pure $
                        VersionLatestRes $
                            HM.union (HM.fromList $ filteredPackages) (HM.fromList packageList)
    where
        selectLatestVersion :: NonEmpty VersionRecord -> Version
        selectLatestVersion vs = NE.head $ (versionRecordNumber <$>) $ NE.sortOn (Down . versionRecordNumber) $ vs