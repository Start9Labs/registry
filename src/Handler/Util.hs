{-# LANGUAGE QuasiQuotes #-}

module Handler.Util where

import Control.Monad.Reader.Has (
    Has,
    MonadReader,
 )
import Data.Attoparsec.Text (Parser, parseOnly)
import Data.String.Interpolate.IsString (i)
import Data.Text qualified as T
import Data.Text.Lazy qualified as TL
import Data.Text.Lazy.Builder qualified as TB
import Lib.PkgRepository (PkgRepo, getHash)
import Lib.Types.AppIndex (PkgId)
import Lib.Types.Emver (
    Version,
    VersionRange,
 )
import Network.HTTP.Types (
    Status,
    status400,
 )
import Startlude (
    Bool (..),
    Either (..),
    Foldable (foldMap),
    Maybe (..),
    Monoid (..),
    Semigroup ((<>)),
    Text,
    decodeUtf8,
    fromMaybe,
    fst,
    isSpace,
    not,
    pure,
    readMaybe,
    ($),
    (.),
    (<$>),
    (>>=),
 )
import UnliftIO (MonadUnliftIO)
import Yesod (
    MonadHandler,
    RenderRoute (..),
    TypedContent (..),
    lookupGetParam,
    sendResponseStatus,
    toContent,
    typePlain,
 )
import Yesod.Core (addHeader)


orThrow :: MonadHandler m => m (Maybe a) -> m a -> m a
orThrow action other =
    action >>= \case
        Nothing -> other
        Just x -> pure x


sendResponseText :: MonadHandler m => Status -> Text -> m a
sendResponseText s = sendResponseStatus s . TypedContent typePlain . toContent


getVersionSpecFromQuery :: MonadHandler m => m VersionRange
getVersionSpecFromQuery = do
    specString <- T.filter (not . isSpace) . fromMaybe "*" <$> lookupGetParam "spec"
    case readMaybe specString of
        Nothing -> sendResponseStatus status400 ("Invalid App Version Specification" :: Text)
        Just t -> pure t


versionPriorityFromQueryIsMin :: MonadHandler m => m Bool
versionPriorityFromQueryIsMin = do
    priorityString <- lookupGetParam "version-priority"
    case priorityString of
        Nothing -> pure False
        (Just "max") -> pure False
        (Just "min") -> pure True
        (Just t) -> sendResponseStatus status400 ("Invalid Version Priority Specification: " <> t)


addPackageHeader :: (MonadUnliftIO m, MonadHandler m, MonadReader r m, Has PkgRepo r) => PkgId -> Version -> m ()
addPackageHeader pkg version = do
    packageHash <- getHash pkg version
    addHeader "X-S9PK-HASH" $ decodeUtf8 packageHash


basicRender :: RenderRoute a => Route a -> Text
basicRender = TL.toStrict . TB.toLazyText . foldMap (mappend (TB.singleton '/') . TB.fromText) . fst . renderRoute


queryParamAs :: MonadHandler m => Text -> Parser a -> m (Maybe a)
queryParamAs k p =
    lookupGetParam k >>= \case
        Nothing -> pure Nothing
        Just x -> case parseOnly p x of
            Left e ->
                sendResponseText status400 [i|Invalid Request! The query parameter '#{k}' failed to parse: #{e}|]
            Right a -> pure (Just a)
