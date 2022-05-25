{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TemplateHaskell #-}
module Handler.Admin where

import           Conduit                        ( (.|)
                                                , runConduit
                                                , sinkFile
                                                )
import           Control.Monad.Reader.Has       ( ask )
import           Control.Monad.Trans.Maybe      ( MaybeT(..) )
import           Data.Aeson                     ( (.:)
                                                , (.=)
                                                , FromJSON(parseJSON)
                                                , ToJSON
                                                , decodeFileStrict
                                                , object
                                                , withObject
                                                )
import           Data.String.Interpolate.IsString
                                                ( i )
import           Database.Persist.Postgresql    ( runSqlPoolNoTransaction )
import           Database.Queries               ( upsertPackageVersion )
import           Foundation
import           Lib.PkgRepository              ( PkgRepo(PkgRepo, pkgRepoFileRoot)
                                                , extractPkg
                                                , getManifestLocation
                                                )
import           Lib.Types.AppIndex             ( PackageManifest(..)
                                                , PkgId(unPkgId)
                                                )
import           Lib.Types.Emver                ( Version(..) )
import           Model                          ( Key(PkgRecordKey, VersionRecordKey) )
import           Network.HTTP.Types             ( status404
                                                , status500
                                                )
import           Startlude                      ( ($)
                                                , (.)
                                                , (<$>)
                                                , Applicative(pure)
                                                , Bool(..)
                                                , Eq
                                                , Maybe(..)
                                                , Show
                                                , SomeException(..)
                                                , asum
                                                , hush
                                                , isNothing
                                                , liftIO
                                                , replicate
                                                , show
                                                , toS
                                                , when
                                                )
import           System.FilePath                ( (<.>)
                                                , (</>)
                                                )
import           UnliftIO                       ( try
                                                , withSystemTempDirectory
                                                )
import           UnliftIO.Directory             ( createDirectoryIfMissing
                                                , removePathForcibly
                                                , renameDirectory
                                                , renameFile
                                                )
import           Util.Shared                    ( orThrow
                                                , sendResponseText
                                                )
import           Yesod                          ( ToJSON(..)
                                                , delete
                                                , getsYesod
                                                , logError
                                                , rawRequestBody
                                                , requireCheckJsonBody
                                                , runDB
                                                )

postPkgUploadR :: Handler ()
postPkgUploadR = do
    withSystemTempDirectory "newpkg" $ \dir -> do
        let path = dir </> "temp" <.> "s9pk"
        runConduit $ rawRequestBody .| sinkFile path
        pool         <- getsYesod appConnPool
        PkgRepo {..} <- ask
        res          <- retry $ extractPkg pool path
        when (isNothing res) $ do
            $logError "Failed to extract package"
            sendResponseText status500 "Failed to extract package"
        PackageManifest {..} <- liftIO (decodeFileStrict (dir </> "manifest.json"))
            `orThrow` sendResponseText status500 "Failed to parse manifest.json"
        renameFile path (dir </> (toS . unPkgId) packageManifestId <.> "s9pk")
        let targetPath = pkgRepoFileRoot </> show packageManifestId </> show packageManifestVersion
        removePathForcibly targetPath
        createDirectoryIfMissing True targetPath
        renameDirectory dir targetPath
    where retry m = runMaybeT . asum $ replicate 3 (MaybeT $ hush <$> try @_ @SomeException m)


data IndexPkgReq = IndexPkgReq
    { indexPkgReqId      :: !PkgId
    , indexPkgReqVersion :: !Version
    }
    deriving (Eq, Show)
instance FromJSON IndexPkgReq where
    parseJSON = withObject "Index Package Request" $ \o -> do
        indexPkgReqId      <- o .: "id"
        indexPkgReqVersion <- o .: "version"
        pure IndexPkgReq { .. }
instance ToJSON IndexPkgReq where
    toJSON IndexPkgReq {..} = object ["id" .= indexPkgReqId, "version" .= indexPkgReqVersion]

postPkgIndexR :: Handler ()
postPkgIndexR = do
    IndexPkgReq {..} <- requireCheckJsonBody
    manifest         <- getManifestLocation indexPkgReqId indexPkgReqVersion
    man              <- liftIO (decodeFileStrict manifest) `orThrow` sendResponseText
        status404
        [i|Could not locate manifest for #{indexPkgReqId}@#{indexPkgReqVersion}|]
    pool <- getsYesod appConnPool
    runSqlPoolNoTransaction (upsertPackageVersion man) pool Nothing

postPkgDeindexR :: Handler ()
postPkgDeindexR = do
    IndexPkgReq {..} <- requireCheckJsonBody
    runDB $ delete (VersionRecordKey (PkgRecordKey indexPkgReqId) indexPkgReqVersion)
