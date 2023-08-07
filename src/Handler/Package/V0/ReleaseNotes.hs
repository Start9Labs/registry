{-# LANGUAGE RecordWildCards #-}

module Handler.Package.V0.ReleaseNotes where

import Data.Aeson (
    KeyValue ((.=)),
    ToJSON (..),
    object,
 )
import Data.Aeson.Key (fromText)
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HM
import Foundation (Handler)
import Handler.Package.V1.Index (getOsVersionCompat)
import Handler.Util (fetchCompatiblePkgVersions)
import Lib.Types.Core (PkgId)
import Lib.Types.Emver (Version)
import Model (VersionRecord (..))
import Startlude (
    Down (..),
    Eq,
    Show,
    Text,
    fst,
    pure,
    show,
    sortOn,
    ($),
    (&&&),
    (.),
    (<$>),
 )
import Yesod (
    ToContent (..),
    ToTypedContent (..),
 )


newtype ReleaseNotes = ReleaseNotes {unReleaseNotes :: HashMap Version Text}
    deriving (Eq, Show)
instance ToJSON ReleaseNotes where
    toJSON ReleaseNotes{..} =
        object [version .= value | (key, value) <- HM.toList unReleaseNotes, let version = fromText $ show key]
instance ToContent ReleaseNotes where
    toContent = toContent . toJSON
instance ToTypedContent ReleaseNotes where
    toTypedContent = toTypedContent . toJSON


getReleaseNotesR :: PkgId -> Handler ReleaseNotes
getReleaseNotesR pkg = do
    osVersion <- getOsVersionCompat
    osCompatibleVersions <- fetchCompatiblePkgVersions osVersion pkg
    pure $ constructReleaseNotesApiRes osCompatibleVersions
    where
        constructReleaseNotesApiRes :: [VersionRecord] -> ReleaseNotes
        constructReleaseNotesApiRes vers = do
            ReleaseNotes $
                HM.fromList $
                    sortOn (Down . fst) $
                        (versionRecordNumber &&& versionRecordReleaseNotes)
                            <$> vers
