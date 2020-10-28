module Util.Shared where

import           Startlude

import           Data.Char
import qualified Data.Text                     as T
import           Network.HTTP.Types
import           Yesod.Core

import           Foundation
import           Lib.Registry
import           Lib.Types.Emver
import           Data.Semigroup

getVersionFromQuery :: KnownSymbol a => FilePath -> Extension a -> Handler (Maybe Version)
getVersionFromQuery rootDir ext = do
    specString <- T.filter (not . isSpace) . fromMaybe "*" <$> lookupGetParam "spec"
    spec       <- case readMaybe specString of
        Nothing -> sendResponseStatus status400 ("Invalid App Version Specification" :: Text)
        Just t  -> pure t
    appVersions <- liftIO $ getAvailableAppVersions rootDir ext
    let satisfactory = filter ((<|| spec) . fst . unRegisteredAppVersion) appVersions
    let best         = getMax <$> foldMap (Just . Max . fst . unRegisteredAppVersion) satisfactory
    pure best
