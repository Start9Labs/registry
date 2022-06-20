{-# LANGUAGE RecordWildCards #-}

module Handler.Package.V0.ReleaseNotes where

import Data.Aeson (ToJSON (..))
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HM
import Database.Queries (fetchAllAppVersions)
import Foundation (Handler, RegistryCtx (..))
import Lib.Types.Core (PkgId)
import Lib.Types.Emver (Version)
import Model (VersionRecord (..))
import Startlude (Down (..), Eq, Show, Text, fst, pure, sortOn, ($), (&&&), (.), (<$>))
import Yesod (ToContent (..), ToTypedContent (..), YesodPersist (runDB), getYesod)


newtype ReleaseNotes = ReleaseNotes {unReleaseNotes :: HashMap Version Text}
    deriving (Eq, Show)
instance ToJSON ReleaseNotes where
    toJSON ReleaseNotes{..} = toJSON unReleaseNotes
instance ToContent ReleaseNotes where
    toContent = toContent . toJSON
instance ToTypedContent ReleaseNotes where
    toTypedContent = toTypedContent . toJSON


getReleaseNotesR :: PkgId -> Handler ReleaseNotes
getReleaseNotesR pkg = do
    appConnPool <- appConnPool <$> getYesod
    versionRecords <- runDB $ fetchAllAppVersions appConnPool pkg
    pure $ constructReleaseNotesApiRes versionRecords
    where
        constructReleaseNotesApiRes :: [VersionRecord] -> ReleaseNotes
        constructReleaseNotesApiRes vers = do
            ReleaseNotes $
                HM.fromList $
                    sortOn (Down . fst) $
                        (versionRecordNumber &&& versionRecordReleaseNotes)
                            <$> vers
