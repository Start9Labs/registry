{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}
module Handler.Admin where

import           Conduit                        ( (.|)
                                                , runConduit
                                                , sinkFile
                                                )
import           Control.Monad.Reader.Has       ( ask )
import           Control.Monad.Trans.Maybe      ( MaybeT(..) )
import           Data.Aeson                     ( (.:)
                                                , FromJSON(parseJSON)
                                                , decodeFileStrict
                                                , withObject
                                                )
import           Data.String.Interpolate.IsString
                                                ( i )
import           Database.Queries               ( upsertPackageVersion )
import           Foundation
import           Lib.PkgRepository              ( PkgRepo(PkgRepo, pkgRepoFileRoot)
                                                , extractPkg
                                                , getManifestLocation
                                                )
import           Lib.Types.AppIndex             ( PackageManifest(..)
                                                , PkgId
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
                                                , Eq
                                                , Show
                                                , SomeException(..)
                                                , asum
                                                , hush
                                                , isNothing
                                                , liftIO
                                                , replicate
                                                , show
                                                , when
                                                )
import           System.FilePath                ( (<.>)
                                                , (</>)
                                                )
import           UnliftIO                       ( try
                                                , withSystemTempDirectory
                                                )
import           UnliftIO.Directory             ( renameDirectory )
import           Util.Shared                    ( orThrow
                                                , sendResponseText
                                                )
import           Yesod                          ( delete
                                                , getsYesod
                                                , logError
                                                , rawRequestBody
                                                , requireCheckJsonBody
                                                , runDB
                                                )

postPkgUploadR :: Handler ()
postPkgUploadR = do
    withSystemTempDirectory "newpkg" $ \path -> do
        runConduit $ rawRequestBody .| sinkFile (path </> "temp" <.> "s9pk")
        pool         <- getsYesod appConnPool
        PkgRepo {..} <- ask
        res          <- retry $ extractPkg pool path
        when (isNothing res) $ do
            $logError "Failed to extract package"
            sendResponseText status500 "Failed to extract package"
        PackageManifest {..} <- liftIO (decodeFileStrict (path </> "manifest.json"))
            `orThrow` sendResponseText status500 "Failed to parse manifest.json"
        renameDirectory path (pkgRepoFileRoot </> show packageManifestId </> show packageManifestVersion)
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

postPkgIndexR :: Handler ()
postPkgIndexR = do
    IndexPkgReq {..} <- requireCheckJsonBody
    manifest         <- getManifestLocation indexPkgReqId indexPkgReqVersion
    man              <- liftIO (decodeFileStrict manifest) `orThrow` sendResponseText
        status404
        [i|Could not locate manifest for #{indexPkgReqId}@#{indexPkgReqVersion}|]
    runDB $ upsertPackageVersion man

postPkgDeindexR :: Handler ()
postPkgDeindexR = do
    IndexPkgReq {..} <- requireCheckJsonBody
    runDB $ delete (VersionRecordKey (PkgRecordKey indexPkgReqId) indexPkgReqVersion)
