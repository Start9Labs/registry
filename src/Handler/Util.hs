{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Handler.Util where

import Control.Monad.Reader.Has (
    Has,
    MonadReader,
 )
import Data.Attoparsec.Text (
    Parser,
    parseOnly,
 )
import Data.String.Interpolate.IsString (
    i,
 )
import Data.Text qualified as T
import Data.Text.Lazy qualified as TL
import Data.Text.Lazy.Builder qualified as TB
import Database.Queries (fetchAllPkgVersions, getVersionPlatform)
import Foundation
import Lib.PkgRepository (
    PkgRepo,
    getHash,
 )
import Lib.Types.Core (PkgId, OsArch (..))
import Lib.Types.Emver (
    Version,
    VersionRange,
    satisfies, parseVersion
 )
import Model (
    UserActivity (..),
    VersionRecord (versionRecordOsVersion, versionRecordDeprecatedAt, versionRecordPkgId), VersionPlatform (versionPlatformDevice),
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
    const,
    decodeUtf8,
    filter,
    flip,
    fromMaybe,
    fst,
    getCurrentTime,
    isSpace,
    liftIO,
    not,
    pure,
    readMaybe,
    void,
    ($),
    (.),
    (<$>),
    (>>=), note, (=<<), catMaybes, all, encodeUtf8, toS, fmap, traceM, show, trace, any, or, (++), IO, putStrLn, map
 )
import UnliftIO (MonadUnliftIO)
import Yesod (
    MonadHandler,
    RenderRoute (..),
    TypedContent (..),
    YesodPersist (runDB),
    getYesod,
    insertRecord,
    liftHandler,
    lookupGetParam,
    sendResponseStatus,
    toContent,
    typePlain,
 )
import Yesod.Core (addHeader, logWarn)
import Lib.Error (S9Error (..))
import Data.Maybe (isJust)
import qualified Data.HashMap.Strict as HM
import Lib.Types.Manifest
import Text.Regex.TDFA ((=~))
import Data.Aeson (eitherDecodeStrict)
import Data.Bifunctor (Bifunctor(first))
import qualified Data.MultiMap as MM
import Startlude (bimap)

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


getVersionFromQuery :: MonadHandler m => m (Maybe Version)
getVersionFromQuery = do
    versionString <- lookupGetParam "version"
    case versionString of
        Nothing -> pure Nothing
        Just v -> case readMaybe v of
            Nothing -> sendResponseStatus status400 ("Invalid Version" :: Text)
            Just t -> pure (Just t)


getHashFromQuery :: MonadHandler m => m (Maybe Text)
getHashFromQuery = lookupGetParam "hash"

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
            Left e -> sendResponseText status400 [i|Invalid Request! The query parameter '#{k}' failed to parse: #{e}|]
            Right a -> pure (Just a)

parseQueryParam :: Text -> (Text -> Either Text a) -> Handler (Maybe a)
parseQueryParam param parser = do
    lookupGetParam param >>= \case
        Nothing -> pure Nothing
        Just x -> case parser x of
            Left e -> do
                let err = InvalidParamsE ("get:" <> param) x
                $logWarn e
                sendResponseStatus status400 err
            Right a -> pure (Just a)

tickleMAU :: Handler ()
tickleMAU = do
    lookupGetParam "server-id" >>= \case
        Nothing -> pure ()
        Just sid -> do
            currentOsVersion <- getOsVersion
            arch <- getOsArch
            now <- liftIO getCurrentTime
            void $ liftHandler $ runDB $ insertRecord $ UserActivity now sid currentOsVersion arch


fetchCompatiblePkgVersions :: Maybe VersionRange -> PkgId -> Handler [VersionRecord]
fetchCompatiblePkgVersions osVersion pkg = do
    appConnPool <- appConnPool <$> getYesod
    versionRecords <- fetchAllPkgVersions appConnPool pkg
    pure $ filter (osPredicate osVersion . versionRecordOsVersion) versionRecords
    where
        osPredicate osV = do
            case osV of
                Nothing -> const True
                Just v -> flip satisfies v

getOsArchQueryLegacy :: Handler (Maybe OsArch)
getOsArchQueryLegacy = parseQueryParam "arch" ((flip $ note . mappend "Invalid 'arch': ") =<< readMaybe)

getOsArchQuery :: Handler (Maybe OsArch)
getOsArchQuery = parseQueryParam "os.arch" ((flip $ note . mappend "Invalid 'arch': ") =<< readMaybe)

getOsArch :: Handler (Maybe OsArch)
getOsArch = do
    osArch <- getOsArchQuery >>= \case
        Just a -> pure $ Just a
        Nothing -> getOsArchQueryLegacy
    pure osArch

getOsVersionLegacy :: Handler (Maybe Version)
getOsVersionLegacy = parseQueryParam "eos-version" ((flip $ note . mappend "Invalid 'eos-version': ") =<< readMaybe)

getOsVersionQuery :: Handler (Maybe Version)
getOsVersionQuery = parseQueryParam "os.version" ((flip $ note . mappend "Invalid 'os.version': ") =<< readMaybe)

getOsVersion :: Handler (Maybe Version)
getOsVersion = do
    osVersion <- getOsVersionQuery >>= \case
        Just a -> pure $ Just a
        Nothing -> getOsVersionLegacy
    pure osVersion

getPkgArch :: Handler [OsArch]
getPkgArch = do 
    arch <- parseQueryParam "hardware.arch" ((flip $ note . mappend "Invalid 'hardware.arch': ") =<< readMaybe)
    case arch of
        Just a -> pure [a]
        Nothing -> do
            getOsArch >>= \case
                Just a -> pure [matchLegacyArch a]
                Nothing -> pure [X86_64, AARCH64]
    where
        matchLegacyArch X86_64 = X86_64
        matchLegacyArch AARCH64 = AARCH64
        matchLegacyArch RASPBERRYPI = AARCH64
        matchLegacyArch X86_64_NONFREE = X86_64
        matchLegacyArch AARCH64_NONFREE = AARCH64

filterDeprecatedVersions :: Version -> (Version -> Bool) -> [VersionRecord] -> [VersionRecord]
filterDeprecatedVersions communityVersion osPredicate vrs = do
    if (osPredicate communityVersion)
        then filter (\v -> not $ isJust $ versionRecordDeprecatedAt v) $ vrs
        else vrs

filterDevices :: (MonadUnliftIO m) => (MM.MultiMap Text Text) -> [(VersionRecord, VersionPlatform)] -> m [VersionRecord]
filterDevices hardwareDevices pkgRecords = do
    pure $ catMaybes $ fmap (compareHd hardwareDevices) pkgRecords
    where
        compareHd :: MM.MultiMap Text Text -> (VersionRecord, VersionPlatform) -> Maybe VersionRecord
        compareHd hd (vr, vp) = case versionPlatformDevice vp of
            Nothing -> do 
                Just vr
            Just d -> if areRegexMatchesEqual hd d
                then Just vr
                else Nothing

regexMatch :: RegexPattern -> Text -> Bool
regexMatch (RegexPattern pattern) text = text =~ pattern

areRegexMatchesEqual :: MM.MultiMap Text Text -> PackageDevice ->  Bool
areRegexMatchesEqual textMap (PackageDevice regexMap) =
    any checkMatch (HM.toList regexMap)
  where
    checkMatch :: (Text, RegexPattern) -> Bool
    checkMatch (key, regexPattern) = 
        case MM.lookup key textMap of
            val -> or $ regexMatch regexPattern <$> val