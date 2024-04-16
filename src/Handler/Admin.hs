{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module Handler.Admin where

import Conduit (
    runConduit,
    sinkFile,
    (.|),
 )
import Control.Monad.Extra
import Control.Monad.Reader.Has (ask)
import Control.Monad.Trans.Maybe (MaybeT (..))
import Data.Aeson (
    FromJSON (parseJSON),
    ToJSON,
    decodeFileStrict,
    object,
    withObject,
    (.:),
    (.:?),
    (.=),
 )
import Data.Conduit.Zlib (ungzip)
import Data.HashMap.Internal.Strict (
    HashMap,
    differenceWith,
    filter,
    fromListWith,
 )
import Data.List (
    null,
    (\\),
 )
import Data.String.Interpolate.IsString (
    i,
 )
import Data.Time
import Database.Persist (
    Entity (entityKey),
    PersistStoreRead (get),
    PersistUniqueRead (getBy),
    PersistUniqueWrite (deleteBy, insertUnique, upsert),
    entityVal,
    insert_,
    selectList,
    (=.), PersistQueryWrite (deleteWhere),
 )
import Database.Persist.Postgresql (runSqlPoolNoTransaction)
import Database.Queries (upsertPackageVersion, upsertPackageVersionPlatform)
import Foundation (
    Handler,
    RegistryCtx (..),
 )
import Handler.Util (
    getHashFromQuery,
    getVersionFromQuery,
    orThrow,
    sendResponseText, checkAdminAllowedPkgs,
 )
import Lib.PkgRepository (
    PkgRepo (PkgRepo, pkgRepoFileRoot),
    extractPkg,
    getManifestLocation,
    getPackages,
    getVersionsFor,
 )
import Lib.Types.Core (PkgId (unPkgId), OsArch)
import Lib.Types.Emver (Version (..))
import Lib.Types.Manifest (PackageManifest (..))
import Model (
    Category (..),
    EntityField (EosHashHash, VersionPlatformArch, VersionPlatformVersionNumber, VersionPlatformPkgId),
    EosHash (EosHash),
    Key (AdminKey, PkgRecordKey, VersionRecordKey),
    PkgCategory (PkgCategory),
    Unique (UniqueName, UniquePkgCategory),
    Upload (..),
    VersionRecord (versionRecordNumber, versionRecordPkgId),
    unPkgRecordKey, AdminPkgs (AdminPkgs),
 )
import Network.HTTP.Types (
    status400,
    status403,
    status404,
    status500,
 )
import Settings
import Startlude (
    Applicative (pure),
    Bool (..),
    Eq,
    Int,
    Maybe (..),
    Show,
    SomeException (..),
    Text,
    asum,
    fromMaybe,
    guarded,
    hush,
    isNothing,
    liftIO,
    not,
    replicate,
    show,
    toS,
    traverse,
    zip,
    ($),
    (&&&),
    (.),
    (.*),
    (<$>),
    (<<$>>),
    (<>),
    (>),
    (&&),
    (||),
    (<=),
 )
import System.FilePath (
    (<.>),
    (</>),
 )
import UnliftIO (
    try,
    withTempDirectory,
 )
import UnliftIO.Directory (
    createDirectoryIfMissing,
    removePathForcibly,
    renameDirectory,
    renameFile,
 )
import Yesod (
    ToJSON (..),
    delete,
    getsYesod,
    logError,
    rawRequestBody,
    requireCheckJsonBody,
    runDB,
    sendResponseStatus,
 )
import Yesod.Auth (YesodAuth (maybeAuthId))
import Yesod.Core.Types (JSONResponse (JSONResponse))
import Database.Persist.Sql (runSqlPool)
import Data.List (elem, length)
import Database.Persist ((==.))
import Network.HTTP.Types.Status (status401)
import Network.HTTP.Types (status200)

postCheckPkgAuthR :: PkgId -> Handler ()
postCheckPkgAuthR pkgId = do
    whitelist <- getsYesod $ whitelist . appSettings
    maybeAuthId >>= \case
        Nothing -> do
            sendResponseText status401 "User not an authorized admin."
        Just name -> do
            if ((length whitelist > 0 && (pkgId `elem` whitelist)) || length whitelist <= 0)
                then do
                    (authorized, newPkg) <- checkAdminAllowedPkgs pkgId name
                    if authorized && not newPkg
                        then sendResponseText status200 "User authorized to upload this package."
                    else if authorized && newPkg
                        -- if pkg is whitelisted and a new upload, add as authorized for this admin user
                        then do
                            runDB $ insert_ (AdminPkgs (AdminKey name) pkgId)
                            sendResponseText status200 "User authorized to upload this package."
                    else sendResponseText status401 "User not authorized to upload this package."
                else sendResponseText status500 "Package does not belong on this registry."

postPkgUploadR :: Handler ()
postPkgUploadR = do
    resourcesTemp <- getsYesod $ (</> "temp") . resourcesDir . appSettings
    whitelist <- getsYesod $ whitelist . appSettings
    createDirectoryIfMissing True resourcesTemp
    withTempDirectory resourcesTemp "newpkg" $ \dir -> do
        let path = dir </> "temp" <.> "s9pk"
        runConduit $ rawRequestBody .| sinkFile path
        pool <- getsYesod appConnPool
        PkgRepo{..} <- ask
        res <- retry $ extractPkg pool path
        when (isNothing res) $ do
            $logError "Failed to extract package"
            sendResponseText status500 "Failed to extract package"
        PackageManifest{..} <- do
            liftIO (decodeFileStrict (dir </> "manifest.json"))
                `orThrow` sendResponseText status500 "Failed to parse manifest.json"
        if ((length whitelist > 0 && (packageManifestId `elem` whitelist)) || length whitelist <= 0)
         then do
            renameFile path (dir </> (toS . unPkgId) packageManifestId <.> "s9pk")
            let targetPath = pkgRepoFileRoot </> show packageManifestId </> show packageManifestVersion
            removePathForcibly targetPath
            createDirectoryIfMissing True targetPath
            renameDirectory dir targetPath
            maybeAuthId >>= \case
                Nothing -> do
                    $logError
                        "Impossible: an unauthenticated user has managed to upload a pacakge to this registry."
                    pure ()
                Just name -> do
                    (authorized, _) <- checkAdminAllowedPkgs packageManifestId name
                    if authorized
                        then do
                            now <- liftIO getCurrentTime
                            runDB $ insert_ (Upload (AdminKey name) (PkgRecordKey packageManifestId)packageManifestVersion now)
                        else sendResponseText status401 "User not authorized to upload this package."
        else sendResponseText status500 "Package does not belong on this registry."
    where
        retry m = runMaybeT . asum $ replicate 3 (MaybeT $ hush <$> try @_ @SomeException m)


postEosUploadR :: Handler ()
postEosUploadR = do
    root <- getsYesod $ (</> "eos") . resourcesDir . appSettings
    maybeVersion <- getVersionFromQuery
    version <- case maybeVersion of
        Nothing -> sendResponseStatus status400 ("Missing Version" :: Text)
        Just v -> pure v
    maybeHash <- getHashFromQuery
    hash <- case maybeHash of
        Nothing -> sendResponseStatus status400 ("Missing Hash" :: Text)
        Just h -> pure h
    resourcesTemp <- getsYesod $ (</> "temp") . resourcesDir . appSettings
    createDirectoryIfMissing True resourcesTemp
    withTempDirectory resourcesTemp "neweos" $ \dir -> do
        let path = dir </> "eos" <.> "img"
        runConduit $ rawRequestBody .| ungzip .| sinkFile path
        void . runDB $ upsert (EosHash version hash) [EosHashHash =. hash]
        let targetPath = root </> show version
        removePathForcibly targetPath
        createDirectoryIfMissing True targetPath
        renameDirectory dir targetPath


data IndexPkgReq = IndexPkgReq
    { indexPkgReqId :: !PkgId
    , indexPkgReqVersion :: !Version
    , indexPkgReqArches :: !(Maybe [OsArch])
    }
    deriving (Eq, Show)
instance FromJSON IndexPkgReq where
    parseJSON = withObject "Index Package Request" $ \o -> do
        indexPkgReqId <- o .: "id"
        indexPkgReqVersion <- o .: "version"
        indexPkgReqArches <- o .:? "arches"
        pure IndexPkgReq{..}
instance ToJSON IndexPkgReq where
    toJSON IndexPkgReq{..} = object ["id" .= indexPkgReqId, "version" .= indexPkgReqVersion]


postPkgIndexR :: Handler ()
postPkgIndexR = do
    IndexPkgReq{..} <- requireCheckJsonBody
    maybeAuthId >>= \case
        Nothing -> do
            $logError
                "Impossible: an unauthenticated user has accessed the index endpoint."
            pure ()
        Just name -> do
            (authorized, _) <- checkAdminAllowedPkgs indexPkgReqId name
            if authorized
                then do
                    manifest <- getManifestLocation indexPkgReqId indexPkgReqVersion
                    man <-
                        liftIO (decodeFileStrict manifest)
                            `orThrow` sendResponseText
                                status404
                                [i|Could not decode manifest for #{indexPkgReqId}@#{indexPkgReqVersion}|]
                    pool <- getsYesod appConnPool
                    runSqlPoolNoTransaction (upsertPackageVersion man) pool Nothing
                    runSqlPool (upsertPackageVersionPlatform indexPkgReqArches man) pool
                else sendResponseText status401 "User not authorized to index this package."

postPkgDeindexR :: Handler ()
postPkgDeindexR = do
    IndexPkgReq{..} <- requireCheckJsonBody
    maybeAuthId >>= \case
        Nothing -> do
            $logError
                "Impossible: an unauthenticated user has accessed the deindex endpoint."
            pure ()
        Just name -> do
            (authorized, _) <- checkAdminAllowedPkgs indexPkgReqId name
            if authorized
                then do
                    case indexPkgReqArches of
                        Nothing -> runDB $ delete (VersionRecordKey (PkgRecordKey indexPkgReqId) indexPkgReqVersion)
                        Just a -> do
                            _ <- traverse (deleteArch indexPkgReqId indexPkgReqVersion) a
                            pure ()
                else sendResponseText status401 "User not authorized to deindex this package."
    where
        deleteArch :: PkgId -> Version -> OsArch -> Handler ()
        deleteArch id v a = runDB $ deleteWhere [VersionPlatformArch ==. a, VersionPlatformVersionNumber ==. v, VersionPlatformPkgId ==. PkgRecordKey id]


newtype PackageList = PackageList {unPackageList :: HashMap PkgId [Version]}
instance FromJSON PackageList where
    parseJSON = fmap PackageList . parseJSON
instance ToJSON PackageList where
    toJSON = toJSON . unPackageList


getPkgDeindexR :: Handler (JSONResponse PackageList)
getPkgDeindexR = do
    dbList <-
        runDB $
            (unPkgRecordKey . versionRecordPkgId &&& (: []) . versionRecordNumber)
                . entityVal
                <<$>> selectList [] []
    let inDb = fromListWith (<>) dbList
    pkgsOnDisk <- getPackages
    onDisk <- fromListWith (<>) . zip pkgsOnDisk <$> traverse getVersionsFor pkgsOnDisk
    pure . JSONResponse . PackageList $ filter (not . null) $ differenceWith (guarded null .* (\\)) onDisk inDb


data AddCategoryReq = AddCategoryReq
    { addCategoryDescription :: !(Maybe Text)
    , addCategoryPriority :: !(Maybe Int)
    }
instance FromJSON AddCategoryReq where
    parseJSON = withObject "AddCategoryReq" $ \o -> do
        addCategoryDescription <- o .:? "description"
        addCategoryPriority <- o .:? "priority"
        pure AddCategoryReq{..}
instance ToJSON AddCategoryReq where
    toJSON AddCategoryReq{..} = object ["description" .= addCategoryDescription, "priority" .= addCategoryPriority]


postCategoryR :: Text -> Handler ()
postCategoryR cat = do
    AddCategoryReq{..} <- requireCheckJsonBody
    now <- liftIO getCurrentTime
    void . runDB $ upsert (Category now cat (fromMaybe "" addCategoryDescription) (fromMaybe 0 addCategoryPriority)) []


deleteCategoryR :: Text -> Handler ()
deleteCategoryR cat = runDB $ deleteBy (UniqueName cat)


postPkgCategorizeR :: Text -> PkgId -> Handler ()
postPkgCategorizeR cat pkg = do
    maybeAuthId >>= \case
        Nothing -> do
            $logError
                "Impossible: an unauthenticated user has accessed the categorize endpoint."
            pure ()
        Just name -> do
            (authorized, _) <- checkAdminAllowedPkgs pkg name
            if authorized
                then runDB $ do
                    catEnt <- getBy (UniqueName cat) `orThrow` sendResponseText status404 [i|Category "#{cat}" does not exist|]
                    _pkgEnt <- get (PkgRecordKey pkg) `orThrow` sendResponseText status404 [i|Package "#{pkg}" does not exist|]
                    now <- liftIO getCurrentTime
                    void $
                        insertUnique (PkgCategory now (PkgRecordKey pkg) (entityKey catEnt))
                            `orThrow` sendResponseText
                                status403
                                [i|Package "#{pkg}" is already assigned to category "#{cat}"|]

                else sendResponseText status401 "User not authorized to categorize this package."

deletePkgCategorizeR :: Text -> PkgId -> Handler ()
deletePkgCategorizeR cat pkg = do
    maybeAuthId >>= \case
        Nothing -> do
            $logError
                "Impossible: an unauthenticated user has accessed the uncategorize endpoint."
            pure ()
        Just name -> do
            (authorized, _) <- checkAdminAllowedPkgs pkg name
            if authorized
                then runDB $ do
                    catEnt <- getBy (UniqueName cat) `orThrow` sendResponseText status404 [i|Category "#{cat}" does not exist|]
                    deleteBy (UniquePkgCategory (PkgRecordKey pkg) (entityKey catEnt))
                else sendResponseText status401 "User not authorized to uncategorize this package."