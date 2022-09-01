{-# LANGUAGE DataKinds #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Handler.Package.V0.S9PK where

import Conduit (awaitForever, (.|))
import Data.Conduit.Binary (sourceFileRange)
import Data.String.Interpolate.IsString (i)
import Data.Text qualified as T
import Database.Queries (
    createMetric,
    fetchAppVersion,
 )
import Foundation (Handler)
import GHC.Show (show)
import Handler.Package.V1.Index (getOsVersionQuery)
import Handler.Util (
    addPackageHeader,
    fetchCompatiblePkgVersions,
    getVersionSpecFromQuery,
    orThrow,
    versionPriorityFromQueryIsMin,
 )
import Lib.Error (S9Error (..))
import Lib.PkgRepository (
    getBestVersion,
    getPackage,
 )
import Lib.Types.Core (
    PkgId (..),
    S9PK,
 )
import Lib.Types.Emver (Version (..))
import Network.HTTP.Types (
    ByteRange (..),
    hRange,
    parseByteRanges,
    status404,
    status416,
 )
import Startlude (
    Applicative (..),
    Maybe (..),
    Num ((-)),
    Text,
    fmap,
    foldr,
    for,
    liftIO,
    pure,
    void,
    ($),
    (.),
    (>>=),
 )
import System.Directory (getFileSize)
import System.FilePath (takeBaseName)
import Yesod (
    Content (..),
    TypedContent,
    YesodPersist (runDB),
    lookupHeader,
    notFound,
    respond,
    sendChunkBS,
    sendResponseStatus,
    typeOctet,
 )
import Yesod.Core (logError)


getAppR :: S9PK -> Handler TypedContent
getAppR file = do
    mRange <-
        lookupHeader hRange >>= \case
            Nothing -> pure Nothing
            Just bs -> case parseByteRanges bs of
                Nothing -> sendResponseStatus status416 ("Range Not Satisfiable" :: Text)
                Just ranges -> pure $ Just ranges
    let pkg = PkgId . T.pack $ takeBaseName (show file)
    osVersion <- getOsVersionQuery
    osCompatibleVersions <- fetchCompatiblePkgVersions osVersion pkg
    versionSpec <- getVersionSpecFromQuery
    preferMin <- versionPriorityFromQueryIsMin
    version <-
        (pure $ getBestVersion versionSpec preferMin osCompatibleVersions)
            `orThrow` sendResponseStatus status404 (NotFoundE [i|#{pkg} satisfying #{versionSpec}|])
    addPackageHeader pkg version
    void $ recordMetrics pkg version
    pkgPath <-
        getPackage pkg version >>= \case
            Nothing -> sendResponseStatus status404 (NotFoundE [i|#{pkg}@#{version}|])
            Just a -> pure a
    case mRange of
        Nothing -> respond typeOctet $ ContentFile pkgPath Nothing
        Just ranges -> do
            composite <- fmap (foldr (*>) (pure ())) $
                for ranges $ \case
                    ByteRangeFrom start -> pure $ sourceFileRange pkgPath (Just start) Nothing
                    ByteRangeFromTo start end -> pure $ sourceFileRange pkgPath (Just start) (Just end)
                    ByteRangeSuffix suffix -> do
                        sz <- liftIO $ getFileSize pkgPath
                        pure $ sourceFileRange pkgPath (Just $ sz - suffix) Nothing
            respond typeOctet $ ContentSource $ composite .| awaitForever sendChunkBS


recordMetrics :: PkgId -> Version -> Handler ()
recordMetrics pkg appVersion = do
    existingVersion <- runDB $ fetchAppVersion pkg appVersion
    case existingVersion of
        Nothing -> do
            $logError [i|#{pkg}@#{appVersion} not found in database|]
            notFound
        Just _ -> runDB $ createMetric pkg appVersion
