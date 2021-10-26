{-# LANGUAGE FlexibleContexts #-}

module Util.Shared where

import           Startlude               hiding ( Handler )

import qualified Data.Text                     as T
import           Network.HTTP.Types
import           Yesod.Core

import           Control.Monad.Reader.Has       ( Has )
import           Foundation
import           Lib.PkgRepository              ( PkgRepo
                                                , getHash
                                                )
import           Lib.Types.AppIndex             ( PkgId )
import           Lib.Types.Emver

getVersionSpecFromQuery :: Handler VersionRange
getVersionSpecFromQuery = do
    specString <- T.filter (not . isSpace) . fromMaybe "*" <$> lookupGetParam "spec"
    case readMaybe specString of
        Nothing -> sendResponseStatus status400 ("Invalid App Version Specification" :: Text)
        Just t  -> pure t

addPackageHeader :: (MonadUnliftIO m, MonadHandler m, MonadReader r m, Has PkgRepo r) => PkgId -> Version -> m ()
addPackageHeader pkg version = do
    packageHash <- getHash pkg version
    addHeader "X-S9PK-HASH" $ decodeUtf8 packageHash

orThrow :: MonadHandler m => m (Maybe a) -> m a -> m a
orThrow action other = action >>= \case
    Nothing -> other
    Just x  -> pure x
