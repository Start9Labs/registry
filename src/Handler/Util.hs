{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

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
    (>>=), note, (=<<), catMaybes, all, traverse, or, encodeUtf8, toS
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
import Startlude (MonadIO)
import Text.Regex.TDFA ((=~))
import Startlude (filterM)
import Database.Persist.Postgresql (ConnectionPool, runSqlPool)
import Data.Aeson (eitherDecodeStrict)
import Data.Bifunctor (Bifunctor(first))

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
            currentEosVersion <- queryParamAs "eos-version" parseVersion
            arch <- getOsArch
            now <- liftIO getCurrentTime
            void $ liftHandler $ runDB $ insertRecord $ UserActivity now sid currentEosVersion arch


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

getPkgArch :: Handler (Maybe [OsArch])
getPkgArch = do 
    arch <- parseQueryParam "hardware.arch" parseArch >>= \case
        Just a -> pure $ Just a
        Nothing -> do
            getOsArch >>= \case
                Just a -> pure $ Just [a]
                Nothing -> pure $ Just []
    pure arch

-- >>> parseArch "[\"aarch64\"]"
-- Right [aarch64]
parseArch :: Text -> Either Text [OsArch]
parseArch = first toS . eitherDecodeStrict . encodeUtf8

filterDeprecatedVersions :: Version -> (Version -> Bool) -> [VersionRecord] -> [VersionRecord]
filterDeprecatedVersions communityVersion osPredicate vrs = do
    if (osPredicate communityVersion)
        then filter (\v -> not $ isJust $ versionRecordDeprecatedAt v) $ vrs
        else vrs

filterDevices :: (MonadUnliftIO m) => ConnectionPool -> (HM.HashMap Text Text) -> [OsArch] -> [VersionRecord] -> m [VersionRecord]
filterDevices pool hardwareDevices arches pkgRecords = do
    res <- filterM compareHd pkgRecords
    pure res
    where
        compareHd pkgRecord = do
            let id = versionRecordPkgId pkgRecord
            platformDetails <- flip runSqlPool pool $ getVersionPlatform id arches
            let pkgDevices = catMaybes $ versionPlatformDevice <$> platformDetails
            t <- traverse (areRegexMatchesEqual hardwareDevices) pkgDevices
            pure $ or t

regexMatch :: RegexPattern -> Text -> Bool
regexMatch (RegexPattern pattern) text = text =~ pattern

areRegexMatchesEqual :: (MonadIO m) => HM.HashMap Text Text -> PackageDevice ->  m Bool
areRegexMatchesEqual textMap (PackageDevice regexMap) =
    -- putStrLn @Text textMap
    pure $ all checkMatch (HM.toList regexMap)
  where
    checkMatch :: (Text, RegexPattern) -> Bool
    checkMatch (key, regexPattern) = case HM.lookup key textMap of
        Just text -> regexMatch regexPattern text
        Nothing   -> False
