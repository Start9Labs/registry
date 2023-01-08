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
    where_,
    val,
    (^.),
    (==.)
 )
import Foundation (Handler, RegistryCtx (appSettings))
import Handler.Package.V0.ReleaseNotes (ReleaseNotes (..))
import Handler.Util (queryParamAs, getArchQuery)
import Lib.Types.Emver (Version (unVersion), Version(Version), parseVersion)
import Model (EntityField (..), OsVersion (..))
import Orphans.Emver ()
import Startlude (Down (..), Eq, Generic, Maybe (..), Ord ((<)), Show, Text, filter, fst, head, pure, sortOn, ($), (&&&), (.), (<$>), (<&>), (<=))
import Yesod (ToContent (toContent), ToTypedContent (..), YesodPersist (runDB), getsYesod)
import Yesod.Core.Types (JSONResponse (..))
import Settings (AppSettings(maxEosVersion))
import Lib.Types.Core (OsArch(RASPBERRYPI))
import Data.Maybe (fromMaybe)


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
    currentEosVersion <- fromMaybe Version { unVersion = (0,3,0,0) } <$> queryParamAs "eos-version" parseVersion
    arch <- fromMaybe RASPBERRYPI <$> getArchQuery 
    maxVersion <- getsYesod $ maxEosVersion . appSettings
    allEosVersions <- runDB $
        select $ do
            vers <- from $ table @OsVersion
            where_ (vers ^. OsVersionArch ==. val (Just arch))
            orderBy [desc (vers ^. OsVersionNumber)]
            pure vers
    let osV = determineMaxEosVersionAvailable maxVersion currentEosVersion $ entityVal <$> allEosVersions
    let mLatest = head osV
    let mappedVersions =
            ReleaseNotes $
                HM.fromList $
                    sortOn (Down . fst) $
                        filter ((<) currentEosVersion . fst) $
                            ((osVersionNumber &&& osVersionReleaseNotes))
                                <$> osV
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