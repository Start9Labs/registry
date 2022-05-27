{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TemplateHaskell #-}
module Handler.Admin where

import           Conduit                        ( (.|)
                                                , runConduit
                                                , sinkFile
                                                )
import           Control.Exception              ( ErrorCall(ErrorCall) )
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
import           Data.HashMap.Internal.Strict   ( HashMap
                                                , differenceWith
                                                , filter
                                                , fromListWith
                                                )
import           Data.List                      ( (\\)
                                                , null
                                                )
import           Data.String.Interpolate.IsString
                                                ( i )
import           Database.Persist               ( entityVal
                                                , insert_
                                                , selectList
                                                )
import           Database.Persist.Postgresql    ( runSqlPoolNoTransaction )
import           Database.Queries               ( upsertPackageVersion )
import           Foundation                     ( Handler
                                                , RegistryCtx(appConnPool)
                                                )
import           Lib.PkgRepository              ( PkgRepo(PkgRepo, pkgRepoFileRoot)
                                                , extractPkg
                                                , getManifestLocation
                                                , getPackages
                                                , getVersionsFor
                                                )
import           Lib.Types.AppIndex             ( PackageManifest(..)
                                                , PkgId(unPkgId)
                                                )
import           Lib.Types.Emver                ( Version(..) )
import           Model                          ( Key(AdminKey, PkgRecordKey, VersionRecordKey)
                                                , Upload(..)
                                                , VersionRecord(versionRecordNumber, versionRecordPkgId)
                                                , unPkgRecordKey
                                                )
import           Network.HTTP.Types             ( status404
                                                , status500
                                                )
import           Startlude                      ( ($)
                                                , (&&&)
                                                , (.)
                                                , (<$>)
                                                , (<<$>>)
                                                , (<>)
                                                , Applicative(pure)
                                                , Bool(..)
                                                , Eq
                                                , Maybe(..)
                                                , Monad((>>=))
                                                , Show
                                                , SomeException(..)
                                                , asum
                                                , fmap
                                                , getCurrentTime
                                                , guarded
                                                , hush
                                                , isNothing
                                                , liftIO
                                                , not
                                                , replicate
                                                , show
                                                , throwIO
                                                , toS
                                                , traverse
                                                , when
                                                , zip
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
import           Yesod.Auth                     ( YesodAuth(maybeAuthId) )
import           Yesod.Core.Types               ( JSONResponse(JSONResponse) )

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
        maybeAuthId >>= \case
            Nothing -> do
                -- TODO: Send this to Matrix
                $logError
                    "The Impossible has happened, an unauthenticated user has managed to upload a pacakge to this registry"
                throwIO $ ErrorCall "Unauthenticated user has uploaded package to registry!!!"
            Just name -> do
                now <- liftIO getCurrentTime
                runDB $ insert_ (Upload (AdminKey name) (PkgRecordKey packageManifestId) packageManifestVersion now)
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

newtype PackageList = PackageList { unPackageList :: HashMap PkgId [Version] }
instance FromJSON PackageList where
    parseJSON = fmap PackageList . parseJSON
instance ToJSON PackageList where
    toJSON = toJSON . unPackageList

getPkgDeindexR :: Handler (JSONResponse PackageList)
getPkgDeindexR = do
    dbList <-
        runDB
        $     (unPkgRecordKey . versionRecordPkgId &&& (: []) . versionRecordNumber)
        .     entityVal
        <<$>> selectList [] []
    let inDb = fromListWith (<>) dbList
    pkgsOnDisk <- getPackages
    onDisk     <- fromListWith (<>) . zip pkgsOnDisk <$> traverse getVersionsFor pkgsOnDisk
    pure . JSONResponse . PackageList $ filter (not . null) $ differenceWith (guarded null .* (\\)) onDisk inDb

(.*) :: (b -> c) -> (a1 -> a2 -> b) -> a1 -> a2 -> c
(.*) = (.) . (.)
