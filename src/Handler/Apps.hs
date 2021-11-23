{-# LANGUAGE TupleSections #-}
{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE QuasiQuotes      #-}
{-# LANGUAGE RecordWildCards  #-}
{-# LANGUAGE TemplateHaskell  #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}

module Handler.Apps where

import           Startlude               hiding ( Handler )

import           Control.Monad.Logger           ( logError )
import qualified Data.Text                     as T
import qualified GHC.Show                       ( Show(..) )
import           Network.HTTP.Types             ( status404 )
import           System.FilePath                ( (<.>)
                                                , takeBaseName
                                                )
import           Yesod.Core                     ( TypedContent
                                                , addHeader
                                                , notFound
                                                , respondSource
                                                , sendChunkBS
                                                , sendResponseStatus
                                                , typeJson
                                                , typeOctet
                                                )
import           Yesod.Persist.Core             ( YesodPersist(runDB) )

import           Conduit                        ( (.|)
                                                , awaitForever
                                                )
import           Data.String.Interpolate.IsString
                                                ( i )
import           Database.Queries               ( createMetric
                                                , fetchApp
                                                , fetchAppVersion
                                                )
import           Foundation                     ( Handler )
import           Lib.Error                      ( S9Error(NotFoundE) )
import           Lib.PkgRepository              ( getBestVersion
                                                , getManifest
                                                , getPackage
                                                )
import           Lib.Registry                   ( S9PK )
import           Lib.Types.AppIndex             ( PkgId(PkgId) )
import           Lib.Types.Emver                ( Version )
import           Util.Shared                    ( addPackageHeader
                                                , getVersionSpecFromQuery
                                                , orThrow
                                                )

data FileExtension = FileExtension FilePath (Maybe String)
instance Show FileExtension where
    show (FileExtension f Nothing ) = f
    show (FileExtension f (Just e)) = f <.> e

getAppManifestR :: PkgId -> Handler TypedContent
getAppManifestR pkg = do
    versionSpec <- getVersionSpecFromQuery
    version     <- getBestVersion pkg versionSpec
        `orThrow` sendResponseStatus status404 (NotFoundE [i|#{pkg} satisfying #{versionSpec}|])
    addPackageHeader pkg version
    (len, src) <- getManifest pkg version
    addHeader "Content-Length" (show len)
    respondSource typeJson $ src .| awaitForever sendChunkBS

getAppR :: S9PK -> Handler TypedContent
getAppR file = do
    let pkg = PkgId . T.pack $ takeBaseName (show file)
    versionSpec <- getVersionSpecFromQuery
    version     <- getBestVersion pkg versionSpec
        `orThrow` sendResponseStatus status404 (NotFoundE [i|#{pkg} satisfying #{versionSpec}|])
    addPackageHeader pkg version
    void $ recordMetrics pkg version
    (len, src) <- getPackage pkg version >>= \case
        Nothing -> sendResponseStatus status404 (NotFoundE [i|#{pkg}@#{version}|])
        Just a  -> pure a
    addHeader "Content-Length" (show len)
    respondSource typeOctet $ src .| awaitForever sendChunkBS


recordMetrics :: PkgId -> Version -> Handler ()
recordMetrics pkg appVersion = do
    sa <- runDB $ fetchApp $ pkg
    case sa of
        Nothing -> do
            $logError $ [i|#{pkg} not found in database|]
            notFound
        Just _ -> do
            existingVersion <- runDB $ fetchAppVersion pkg appVersion
            case existingVersion of
                Nothing -> do
                    $logError $ [i|#{pkg}@#{appVersion} not found in database|]
                    notFound
                Just _ -> runDB $ createMetric pkg appVersion

