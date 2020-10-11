module Util.Shared where

import           Startlude

import           Data.Char
import qualified Data.Text                     as T
import           Network.HTTP.Types
import           Yesod.Core

import           Foundation
import           Lib.Registry
import           Lib.Semver
import           Lib.Types.Semver

getVersionFromQuery :: KnownSymbol a => FilePath -> Extension a -> Handler (Maybe AppVersion)
getVersionFromQuery rootDir ext = do
    specString <- T.filter (not . isSpace) . fromMaybe "*" <$> lookupGetParam "spec"
    spec       <- case readMaybe specString of
        Nothing -> sendResponseStatus status400 ("Invalid App Version Specification" :: Text)
        Just t  -> pure t
    appVersions <- liftIO $ getAvailableAppVersions rootDir ext
    pure $ version <$> getSpecifiedAppVersion spec appVersions