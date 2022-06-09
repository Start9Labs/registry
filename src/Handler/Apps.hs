{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}

module Handler.Apps where

import Startlude (
    Applicative (pure),
    FilePath,
    Maybe (..),
    Monad ((>>=)),
    Show,
    String,
    show,
    void,
    ($),
    (.),
 )

import Control.Monad.Logger (logError)
import Data.Text qualified as T
import GHC.Show qualified (Show (..))
import Network.HTTP.Types (status404)
import System.FilePath (
    takeBaseName,
    (<.>),
 )
import Yesod.Core (
    Content (ContentFile),
    TypedContent,
    addHeader,
    notFound,
    respond,
    respondSource,
    sendChunkBS,
    sendResponseStatus,
    typeJson,
    typeOctet,
 )
import Yesod.Persist.Core (YesodPersist (runDB))

import Conduit (
    awaitForever,
    (.|),
 )
import Data.String.Interpolate.IsString (
    i,
 )
import Database.Queries (
    createMetric,
    fetchApp,
    fetchAppVersion,
 )
import Foundation (Handler)
import Handler.Util (addPackageHeader, getVersionSpecFromQuery, orThrow, versionPriorityFromQueryIsMin)
import Lib.Error (S9Error (NotFoundE))
import Lib.PkgRepository (
    getBestVersion,
    getManifest,
    getPackage,
 )
import Lib.Registry (S9PK)
import Lib.Types.AppIndex (PkgId (PkgId))
import Lib.Types.Emver (Version)


data FileExtension = FileExtension !FilePath !(Maybe String)
instance Show FileExtension where
    show (FileExtension f Nothing) = f
    show (FileExtension f (Just e)) = f <.> e


getAppManifestR :: PkgId -> Handler TypedContent
getAppManifestR pkg = do
    versionSpec <- getVersionSpecFromQuery
    preferMin <- versionPriorityFromQueryIsMin
    version <-
        getBestVersion pkg versionSpec preferMin
            `orThrow` sendResponseStatus status404 (NotFoundE [i|#{pkg} satisfying #{versionSpec}|])
    addPackageHeader pkg version
    (len, src) <- getManifest pkg version
    addHeader "Content-Length" (show len)
    respondSource typeJson $ src .| awaitForever sendChunkBS


getAppR :: S9PK -> Handler TypedContent
getAppR file = do
    let pkg = PkgId . T.pack $ takeBaseName (show file)
    versionSpec <- getVersionSpecFromQuery
    preferMin <- versionPriorityFromQueryIsMin
    version <-
        getBestVersion pkg versionSpec preferMin
            `orThrow` sendResponseStatus status404 (NotFoundE [i|#{pkg} satisfying #{versionSpec}|])
    addPackageHeader pkg version
    void $ recordMetrics pkg version
    pkgPath <-
        getPackage pkg version >>= \case
            Nothing -> sendResponseStatus status404 (NotFoundE [i|#{pkg}@#{version}|])
            Just a -> pure a
    respond typeOctet $ ContentFile pkgPath Nothing


recordMetrics :: PkgId -> Version -> Handler ()
recordMetrics pkg appVersion = do
    sa <- runDB $ fetchApp pkg
    case sa of
        Nothing -> do
            $logError [i|#{pkg} not found in database|]
            notFound
        Just _ -> do
            existingVersion <- runDB $ fetchAppVersion pkg appVersion
            case existingVersion of
                Nothing -> do
                    $logError [i|#{pkg}@#{appVersion} not found in database|]
                    notFound
                Just _ -> runDB $ createMetric pkg appVersion
