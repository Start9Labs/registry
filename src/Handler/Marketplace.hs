{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Redundant <$>" #-}

module Handler.Marketplace where

import Startlude (
    Applicative (pure, (*>)),
    Bool (True),
    ByteString,
    Down (Down),
    Either (Left, Right),
    FilePath,
    Foldable (foldMap),
    Functor (fmap),
    Int,
    Maybe (..),
    Monad ((>>=)),
    MonadIO,
    MonadReader,
    Monoid (mappend),
    Num ((*), (-)),
    Ord ((<)),
    ReaderT (runReaderT),
    Text,
    Traversable (traverse),
    catMaybes,
    const,
    decodeUtf8,
    encodeUtf8,
    filter,
    flip,
    for_,
    fromMaybe,
    fst,
    head,
    headMay,
    id,
    maybe,
    partitionEithers,
    readMaybe,
    show,
    snd,
    void,
    ($),
    (&&&),
    (.),
    (<$>),
    (<&>),
 )

import Conduit (
    dropC,
    runConduit,
    sinkList,
    takeC,
    (.|),
 )
import Control.Monad.Logger (
    MonadLogger,
    logWarn,
 )
import Control.Monad.Reader.Has (
    Has,
    ask,
 )
import Crypto.Hash (SHA256)
import Crypto.Hash.Conduit (hashFile)
import Data.Aeson (
    decode,
    eitherDecode,
    eitherDecodeStrict,
 )
import Data.Attoparsec.Text qualified as Atto

import Data.Attoparsec.Text (
    Parser,
    parseOnly,
 )
import Data.ByteArray.Encoding (
    Base (..),
    convertToBase,
 )
import Data.ByteString.Base64 (encodeBase64)
import Data.ByteString.Lazy qualified as LBS
import Data.Conduit.List qualified as CL
import Data.HashMap.Strict qualified as HM
import Data.List (
    lookup,
    sortOn,
 )
import Data.String.Interpolate.IsString (
    i,
 )
import Data.Text qualified as T
import Data.Text.Lazy qualified as TL
import Data.Text.Lazy.Builder qualified as TB
import Database.Esqueleto.Experimental (
    Entity (entityKey, entityVal),
    SqlBackend,
    asc,
    desc,
    from,
    orderBy,
    select,
    table,
    (^.),
 )
import Database.Marketplace (
    collateVersions,
    fetchAllAppVersions,
    fetchLatestApp,
    getPkgData,
    getPkgDependencyData,
    searchServices,
    zipCategories,
    zipDependencyVersions,
 )
import Database.Persist (
    PersistUniqueRead (getBy),
    insertUnique,
 )
import Foundation (
    Handler,
    RegistryCtx (appConnPool, appSettings),
    Route (InstructionsR, LicenseR),
 )
import Handler.Util (getVersionSpecFromQuery)
import Lib.Error (S9Error (..))
import Lib.PkgRepository (
    PkgRepo,
    getIcon,
    getManifest,
 )
import Lib.Types.AppIndex (PkgId)
import Lib.Types.Emver (
    Version,
    VersionRange,
    parseRange,
    parseVersion,
    satisfies,
 )
import Model (
    Category (..),
    EntityField (..),
    EosHash (EosHash, eosHashHash),
    Key (PkgRecordKey, unPkgRecordKey),
    OsVersion (..),
    PkgRecord (..),
    Unique (UniqueVersion),
    VersionRecord (..),
 )
import Network.HTTP.Types (
    status400,
    status404,
 )
import Protolude.Unsafe (unsafeFromJust)
import Settings (AppSettings (marketplaceName, resourcesDir))
import System.FilePath ((</>))
import UnliftIO.Async (mapConcurrently)
import UnliftIO.Directory (listDirectory)
import Util.Shared (
    filterDependencyBestVersion,
    filterDependencyOsCompatible,
    filterLatestVersionFromSpec,
    filterPkgOsCompatible,
 )
import Yesod.Core (
    Content (ContentFile),
    MonadHandler,
    MonadResource,
    RenderRoute (renderRoute),
    TypedContent,
    YesodRequest (..),
    addHeader,
    getRequest,
    getYesod,
    getsYesod,
    lookupGetParam,
    respond,
    sendResponseStatus,
    typeOctet,
 )
import Yesod.Core.Types (JSONResponse (..))
import Yesod.Persist (YesodDB)
import Yesod.Persist.Core (YesodPersist (runDB))


queryParamAs :: MonadHandler m => Text -> Parser a -> m (Maybe a)
queryParamAs k p =
    lookupGetParam k >>= \case
        Nothing -> pure Nothing
        Just x -> case parseOnly p x of
            Left e ->
                sendResponseStatus @_ @Text status400 [i|Invalid Request! The query parameter '#{k}' failed to parse: #{e}|]
            Right a -> pure (Just a)


getInfoR :: Handler (JSONResponse InfoRes)
getInfoR = do
    name <- getsYesod $ marketplaceName . appSettings
    allCategories <- runDB $
        select $ do
            cats <- from $ table @Category
            orderBy [asc (cats ^. CategoryPriority)]
            pure cats
    pure $ JSONResponse $ InfoRes name $ categoryName . entityVal <$> allCategories


getEosVersionR :: Handler (JSONResponse (Maybe EosRes))
getEosVersionR = do
    eosVersion <- queryParamAs "eos-version" parseVersion
    allEosVersions <- runDB $
        select $ do
            vers <- from $ table @OsVersion
            orderBy [desc (vers ^. OsVersionCreatedAt)]
            pure vers
    let osV = entityVal <$> allEosVersions
    let mLatest = head osV
    let mappedVersions =
            ReleaseNotes $
                HM.fromList $
                    sortOn (Down . fst) $
                        filter (maybe (const True) (<) eosVersion . fst) $
                            (\v -> (osVersionNumber v, osVersionReleaseNotes v))
                                <$> osV
    pure . JSONResponse $
        mLatest <&> \latest ->
            EosRes
                { eosResVersion = osVersionNumber latest
                , eosResHeadline = osVersionHeadline latest
                , eosResReleaseNotes = mappedVersions
                }


getReleaseNotesR :: PkgId -> Handler ReleaseNotes
getReleaseNotesR pkg = do
    appConnPool <- appConnPool <$> getYesod
    versionRecords <- runDB $ fetchAllAppVersions appConnPool pkg
    pure $ constructReleaseNotesApiRes versionRecords
    where
        constructReleaseNotesApiRes :: [VersionRecord] -> ReleaseNotes
        constructReleaseNotesApiRes vers = do
            ReleaseNotes $
                HM.fromList $
                    sortOn (Down . fst) $
                        (versionRecordNumber &&& versionRecordReleaseNotes)
                            <$> vers


getEosR :: Handler TypedContent
getEosR = do
    spec <- getVersionSpecFromQuery
    root <- getsYesod $ (</> "eos") . resourcesDir . appSettings
    subdirs <- listDirectory root
    let (failures, successes) = partitionEithers $ Atto.parseOnly parseVersion . T.pack <$> subdirs
    for_ failures $ \f -> $logWarn [i|Emver Parse Failure for EOS: #{f}|]
    let mVersion = headMay . sortOn Down . filter (`satisfies` spec) $ successes
    case mVersion of
        Nothing -> sendResponseStatus status404 (NotFoundE [i|EOS version satisfying #{spec}|])
        Just version -> do
            let imgPath = root </> show version </> "eos.img"
            h <- runDB $ retrieveHash version imgPath
            addHeader "x-eos-hash" h
            respond typeOctet $ ContentFile imgPath Nothing
    where
        retrieveHash :: Version -> FilePath -> YesodDB RegistryCtx Text
        retrieveHash v fp = do
            mHash <- getBy (UniqueVersion v)
            case mHash of
                Just h -> pure . eosHashHash . entityVal $ h
                Nothing -> do
                    h <- hashFile @_ @SHA256 fp
                    let t = decodeUtf8 $ convertToBase Base16 h
                    void $ insertUnique (EosHash v t) -- lazily populate
                    pure t


-- TODO refactor with conduit
getVersionLatestR :: Handler VersionLatestRes
getVersionLatestR = do
    getParameters <- reqGetParams <$> getRequest
    case lookup "ids" getParameters of
        Nothing -> sendResponseStatus status400 (InvalidParamsE "get:ids" "<MISSING>")
        Just packages -> case eitherDecode $ LBS.fromStrict $ encodeUtf8 packages of
            Left _ -> sendResponseStatus status400 (InvalidParamsE "get:ids" packages)
            Right p -> do
                let packageList = (,Nothing) <$> p
                found <- runDB $ traverse fetchLatestApp $ fst <$> packageList
                pure $
                    VersionLatestRes $
                        HM.union
                            ( HM.fromList $
                                ( \v ->
                                    (unPkgRecordKey . entityKey $ fst v, Just $ versionRecordNumber $ entityVal $ snd v)
                                )
                                    <$> catMaybes found
                            )
                            $ HM.fromList packageList
