{-# LANGUAGE DataKinds #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Handler.Package.V0.S9PK where

import Data.String.Interpolate.IsString (
    i,
 )
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
    filterOsCompat,
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
import Network.HTTP.Types (status404)
import Startlude (
    Maybe (..),
    pure,
    void,
    ($),
    (.),
    (>>=),
 )
import System.FilePath (takeBaseName)
import Yesod (
    Content (..),
    TypedContent,
    YesodPersist (runDB),
    notFound,
    respond,
    sendResponseStatus,
    typeOctet,
 )
import Yesod.Core (logError)


getAppR :: S9PK -> Handler TypedContent
getAppR file = do
    let pkg = PkgId . T.pack $ takeBaseName (show file)
    osVersion <- getOsVersionQuery
    osCompatibleVersions <- filterOsCompat osVersion pkg
    versionSpec <- getVersionSpecFromQuery
    preferMin <- versionPriorityFromQueryIsMin
    version <-
        getBestVersion versionSpec preferMin osCompatibleVersions
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
    existingVersion <- runDB $ fetchAppVersion pkg appVersion
    case existingVersion of
        Nothing -> do
            $logError [i|#{pkg}@#{appVersion} not found in database|]
            notFound
        Just _ -> runDB $ createMetric pkg appVersion
