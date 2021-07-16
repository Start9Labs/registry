{-# LANGUAGE TemplateHaskell  #-}

module Util.Shared where

import           Startlude hiding (Handler)

import qualified Data.Text                     as T
import           Network.HTTP.Types
import           Yesod.Core

import           Foundation
import           Lib.Registry
import           Lib.Types.Emver
import           Data.Semigroup
import Lib.External.AppMgr
import Lib.Error

getVersionFromQuery :: KnownSymbol a => FilePath -> Extension a -> Handler (Maybe Version)
getVersionFromQuery rootDir ext = do
    specString <- T.filter (not . isSpace) . fromMaybe "*" <$> lookupGetParam "spec"
    spec       <- case readMaybe specString of
        Nothing -> sendResponseStatus status400 ("Invalid App Version Specification" :: Text)
        Just t  -> pure t
    getBestVersion rootDir ext spec

getBestVersion :: (MonadIO m, KnownSymbol a, MonadLogger m) => FilePath -> Extension a -> VersionRange -> m (Maybe Version)
getBestVersion rootDir ext spec = do
    -- @TODO change to db query?
    appVersions <- liftIO $ getAvailableAppVersions rootDir ext
    $logInfo $ show appVersions
    $logInfo $ show spec
    $logInfo $ show ext
    let satisfactory = filter ((<|| spec) . fst . unRegisteredAppVersion) appVersions
    let best         = getMax <$> foldMap (Just . Max . fst . unRegisteredAppVersion) satisfactory
    $logInfo $ show satisfactory
    $logInfo $ show best
    pure best

addPackageHeader :: (MonadHandler m, KnownSymbol a) => FilePath -> FilePath -> Extension a -> m ()
addPackageHeader appMgrDir appDir appExt = do
    packageHash <- handleS9ErrT $ getPackageHash appMgrDir appDir appExt
    addHeader "X-S9PK-HASH" $ decodeUtf8 packageHash