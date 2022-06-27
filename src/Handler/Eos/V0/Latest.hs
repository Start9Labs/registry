{-# LANGUAGE RecordWildCards #-}

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
import Foundation (Handler)
import Handler.Package.V0.ReleaseNotes (ReleaseNotes (..))
import Handler.Util (queryParamAs, tickleMAU)
import Lib.Types.Emver (Version, parseVersion)
import Model (EntityField (..), OsVersion (..))
import Orphans.Emver ()
import Startlude (Bool (..), Down (..), Eq, Generic, Maybe, Ord ((<)), Show, Text, const, filter, fst, head, maybe, pure, sortOn, ($), (&&&), (.), (<$>), (<&>))
import Yesod (ToContent (toContent), ToTypedContent (..), YesodPersist (runDB))
import Yesod.Core.Types (JSONResponse (..))


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
    eosVersion <- queryParamAs "eos-version" parseVersion
    allEosVersions <- runDB $
        select $ do
            vers <- from $ table @OsVersion
            orderBy [desc (vers ^. OsVersionCreatedAt)]
            pure vers
    let osV = entityVal <$> allEosVersions
    let mLatest = head osV
    let mappedVersions =
            ReleaseNotes $
                HM.fromList $
                    sortOn (Down . fst) $
                        filter (maybe (const True) (<) eosVersion . fst) $
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
