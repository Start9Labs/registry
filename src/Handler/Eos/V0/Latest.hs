{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Handler.Eos.V0.Latest where

import Data.Aeson (ToJSON (toJSON), object, (.=))
import Data.HashMap.Strict qualified as HM
import Database.Esqueleto.Experimental (
    Entity (entityVal),
    desc,
    from,
    orderBy,
    select,
    table,
    (^.),
 )
import Foundation (Handler, RegistryCtx (appSettings))
import Handler.Package.V0.ReleaseNotes (ReleaseNotes (..))
import Handler.Util (queryParamAs, tickleMAU)
import Lib.Types.Emver (Version, parseVersion)
import Model (EntityField (..), OsVersion (..))
import Orphans.Emver ()
import Startlude (Bool (..), Down (..), Eq, Generic, Maybe (..), Ord ((<)), Show, Text, const, filter, fst, head, maybe, pure, sortOn, ($), (&&&), (.), (<$>), (<&>), (<=))
import Yesod (ToContent (toContent), ToTypedContent (..), YesodPersist (runDB), getsYesod, sendResponseStatus)
import Yesod.Core.Types (JSONResponse (..))
import Settings (AppSettings(maxEosVersion))
import Network.HTTP.Types (status400)
import Lib.Error (S9Error(InvalidParamsE))


data EosRes = EosRes
    { eosResVersion :: !Version
    , eosResHeadline :: !Text
    , eosResReleaseNotes :: !ReleaseNotes
    }
    deriving (Eq, Show, Generic)
instance ToJSON EosRes where
    toJSON EosRes{..} =
        object ["version" .= eosResVersion, "headline" .= eosResHeadline, "release-notes" .= eosResReleaseNotes]
instance ToContent EosRes where
    toContent = toContent . toJSON
instance ToTypedContent EosRes where
    toTypedContent = toTypedContent . toJSON


getEosVersionR :: Handler (JSONResponse (Maybe EosRes))
getEosVersionR = do
    currentEosVersion <- queryParamAs "eos-version" parseVersion
    case currentEosVersion of
        Nothing -> sendResponseStatus status400 (InvalidParamsE "Param is required" "eos-version")
        Just currentEosVersion' -> do
            maxVersion <- getsYesod $ maxEosVersion . appSettings
            allEosVersions <- runDB $
                select $ do
                    vers <- from $ table @OsVersion
                    orderBy [desc (vers ^. OsVersionNumber)]
                    pure vers
            let osV = determineMaxEosVersionAvailable maxVersion currentEosVersion' $ entityVal <$> allEosVersions
            let mLatest = head osV
            let mappedVersions =
                    ReleaseNotes $
                        HM.fromList $
                            sortOn (Down . fst) $
                                filter (maybe (const True) (<) currentEosVersion . fst) $
                                    ((osVersionNumber &&& osVersionReleaseNotes))
                                        <$> osV
            tickleMAU
            pure . JSONResponse $
                mLatest <&> \latest ->
                    EosRes
                        { eosResVersion = osVersionNumber latest
                        , eosResHeadline = osVersionHeadline latest
                        , eosResReleaseNotes = mappedVersions
                        }

determineMaxEosVersionAvailable ::  Version -> Version -> [OsVersion] -> [OsVersion]
determineMaxEosVersionAvailable maxEosVersion currentEosVersion versions = do
    if (currentEosVersion < maxEosVersion)
        then sortOn (Down . osVersionNumber) $ filter (\v -> osVersionNumber v <= maxEosVersion) $ versions
        else versions