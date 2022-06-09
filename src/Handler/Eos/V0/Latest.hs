{-# LANGUAGE RecordWildCards #-}

module Handler.Eos.V0.Latest where

import Data.Aeson (ToJSON (toJSON), object, (.=))
import Handler.Package.V0.ReleaseNotes (ReleaseNotes)
import Lib.Types.Emver (Version)
import Orphans.Emver ()
import Startlude (Eq, Generic, Show, Text, (.))
import Yesod (ToContent (toContent), ToTypedContent (..))


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