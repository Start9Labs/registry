{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cli.Cli (
    cliMain,
) where

import Conduit (
    foldC,
    runConduit,
    (.|),
 )
import Control.Monad.Logger (
    LogLevel (..),
    MonadLogger (monadLoggerLog),
    MonadLoggerIO (askLoggerIO),
    ToLogStr,
    fromLogStr,
    toLogStr,
 )
import Crypto.Hash (
    SHA256 (SHA256),
    hashWith,
 )
import Crypto.Hash.Conduit (hashFile)
import Data.Aeson (
    ToJSON,
    eitherDecodeStrict,
 )
import Data.ByteArray.Encoding (
    Base (..),
    convertToBase,
 )
import Data.ByteString.Char8 qualified as B8
import Data.ByteString.Lazy qualified as LB
import Data.Conduit.Process (readProcess)
import Data.Default
import Data.Functor.Contravariant (contramap)
import Data.HashMap.Internal.Strict (
    HashMap,
    delete,
    empty,
    insert,
    lookup,
    traverseWithKey,
 )
import Data.String.Interpolate.IsString (
    i,
 )
import Data.Text (toLower)
import Dhall (
    Encoder (embed),
    FromDhall (..),
    Generic,
    ToDhall (..),
    auto,
    inject,
    inputFile,
 )
import Dhall.Core (pretty)
import Handler.Admin (
    AddCategoryReq (AddCategoryReq),
    IndexPkgReq (IndexPkgReq),
    PackageList (..),
 )
import Lib.External.AppMgr (sourceManifest)
import Lib.Types.Core (PkgId (..))
import Lib.Types.Emver (Version (..))
import Lib.Types.Manifest (PackageManifest (..))
import Network.HTTP.Client.Conduit (
    StreamFileStatus (StreamFileStatus, fileSize, readSoFar),
    applyBasicAuth,
    httpLbs,
    observedStreamFile,
 )
import Network.HTTP.Client.TLS (newTlsManager)
import Network.HTTP.Simple (
    getResponseBody,
    getResponseStatus,
    httpJSON,
    httpLBS,
    parseRequest,
    setRequestBody,
    setRequestBodyJSON,
    setRequestHeaders,
    setRequestQueryString,
 )
import Network.HTTP.Types (status200)
import Network.URI (
    URI,
    parseURI,
 )
import Options.Applicative (
    Alternative ((<|>)),
    Applicative (liftA2, pure, (<*>)),
    Parser,
    ParserInfo,
    auto,
    command,
    execParser,
    fullDesc,
    help,
    helper,
    info,
    liftA3,
    long,
    mappend,
    metavar,
    option,
    optional,
    progDesc,
    short,
    strArgument,
    strOption,
    subparser,
    switch,
    (<$>),
    (<**>),
 )
import Rainbow (
    Chunk,
    Radiant,
    blue,
    chunk,
    fore,
    green,
    magenta,
    putChunk,
    putChunkLn,
    red,
    white,
    yellow,
 )
import Startlude (
    Bool (..),
    ConvertText (toS),
    Either (..),
    Eq (..),
    ExitCode (..),
    FilePath,
    IO,
    Int,
    IsString (..),
    Maybe (..),
    Monad ((>>=)),
    Num (fromInteger),
    ReaderT (runReaderT),
    Semigroup ((<>)),
    Show,
    String,
    appendFile,
    const,
    decodeUtf8,
    exitWith,
    filter,
    flip,
    fmap,
    for,
    for_,
    fromIntegral,
    fromMaybe,
    fst,
    headMay,
    not,
    panic,
    show,
    snd,
    unlessM,
    void,
    when,
    writeFile,
    zip,
    ($),
    ($>),
    (&),
    (.),
    (<&>),
 )
import System.Directory (
    createDirectoryIfMissing,
    doesPathExist,
    getCurrentDirectory,
    getFileSize,
    getHomeDirectory,
    listDirectory,
 )
import System.FilePath (
    takeDirectory,
    takeExtension,
    (</>),
 )
import System.ProgressBar (
    Progress (..),
    defStyle,
    newProgressBar,
    updateProgress,
 )
import Yesod (
    logError,
    logWarn,
 )


data Upload = Upload
    { publishRepoName :: !String
    , publishPkg :: !(Maybe FilePath)
    , publishIndex :: !Bool
    }
    deriving (Show)


data EosUpload = EosUpload
    { eosRepoName :: !String
    , eosPath :: !FilePath
    , eosVersion :: !Version
    }
    deriving (Show)


newtype PublishCfg = PublishCfg
    { publishCfgRepos :: HashMap String PublishCfgRepo
    }
    deriving (Generic)
instance FromDhall PublishCfg
instance ToDhall PublishCfg
instance Default PublishCfg where
    def = PublishCfg empty


data PublishCfgRepo = PublishCfgRepo
    { publishCfgRepoLocation :: !URI
    , publishCfgRepoUser :: !String
    , publishCfgRepoPass :: !String
    }
    deriving (Show, Generic)
instance FromDhall PublishCfgRepo
instance ToDhall PublishCfgRepo


instance FromDhall URI where
    autoWith norm = fromMaybe (panic "Invalid URI for publish target") . parseURI <$> autoWith norm


instance ToDhall URI where
    injectWith norm = contramap (show @_ @String) (injectWith norm)


instance IsString URI where
    fromString = fromMaybe (panic "Invalid URI for publish target") . parseURI


data Shell = Bash | Fish | Zsh deriving (Show)
data Command
    = CmdInit !(Maybe Shell)
    | CmdRegAdd !String !PublishCfgRepo
    | CmdRegDel !String
    | CmdRegList
    | CmdUpload !Upload
    | CmdIndex !String !String !Version !Bool
    | CmdListUnindexed !String
    | CmdCatAdd !String !String !(Maybe String) !(Maybe Int)
    | CmdCatDel !String !String
    | CmdPkgCatAdd !String !PkgId !String
    | CmdPkgCatDel !String !PkgId !String
    | CmdEosUpload !EosUpload
    deriving (Show)


cfgLocation :: IO FilePath
cfgLocation = getHomeDirectory <&> \d -> d </> ".embassy/publish.dhall"


parseInit :: Parser (Maybe Shell)
parseInit = subparser $ command "init" (info go $ progDesc "Initializes embassy-publish config") <> metavar "init"
    where
        shells = [Bash, Fish, Zsh]
        go = headMay . fmap fst . filter snd . zip shells <$> for shells (switch . long . toS . toLower . show)


parsePublish :: Parser Upload
parsePublish =
    subparser $
        command "upload" (info go $ progDesc "Publishes a .s9pk to a remote registry")
            <> metavar
                "upload"
    where
        go =
            liftA3
                Upload
                (strOption (short 't' <> long "target" <> metavar "NAME" <> help "Name of registry in publish.dhall"))
                ( optional $
                    strOption
                        (short 'p' <> long "package" <> metavar "S9PK" <> help "File path of the package to publish")
                )
                (switch (short 'i' <> long "index" <> help "Index the package after uploading"))


parseRepoAdd :: Parser Command
parseRepoAdd = subparser $ command "add" (info go $ progDesc "Add a registry to your config") <> metavar "add"
    where
        go :: Parser Command
        go =
            let publishCfgRepoLocation =
                    strOption (short 'l' <> long "location" <> metavar "REGISTRY_URL" <> help "Registry URL")
                publishCfgRepoUser =
                    strOption
                        (short 'u' <> long "username" <> metavar "USERNAME" <> help "Admin username for this registry")
                publishCfgRepoPass =
                    strOption
                        (short 'p' <> long "password" <> metavar "PASSWORD" <> help "Admin password for this registry")
                name =
                    strOption
                        ( short 'n' <> long "name" <> metavar "REGISTRY_NAME"
                            <> help
                                "Name to reference this registry in the future"
                        )
                r = PublishCfgRepo <$> publishCfgRepoLocation <*> publishCfgRepoUser <*> publishCfgRepoPass
             in liftA2 CmdRegAdd name r


parseRepoDel :: Parser String
parseRepoDel = subparser $ command "rm" (info go $ progDesc "Remove a registry from your config") <> metavar "rm"
    where
        go =
            strOption
                ( short 'n' <> long "name" <> metavar "REGISTRY_NAME"
                    <> help
                        "Registry name chosen when this was originally configured"
                )


parseRepoList :: Parser ()
parseRepoList = subparser $ command "ls" (info (pure ()) $ progDesc "List registries in your config") <> metavar "ls"


parseIndex :: Parser Command
parseIndex =
    subparser $
        command "index" (info (parseIndexHelper True) $ progDesc "Indexes an existing package version")
            <> metavar "index"


parseDeindex :: Parser Command
parseDeindex =
    subparser $
        command "deindex" (info (parseIndexHelper False) $ progDesc "Deindexes an existing package version")
            <> metavar "deindex"


parseIndexHelper :: Bool -> Parser Command
parseIndexHelper b =
    CmdIndex
        <$> strOption (short 't' <> long "target" <> metavar "REGISTRY_NAME")
        <*> strArgument (metavar "PKG")
        <*> strArgument (metavar "VERSION")
        <*> pure b


parseListUnindexed :: Parser String
parseListUnindexed =
    subparser $
        command
            "list-unindexed"
            ( info (strOption (short 't' <> long "target" <> metavar "REGISTRY_NAME")) $
                progDesc "Lists unindexed package versions on target registry"
            )


parseCommand :: Parser Command
parseCommand =
    (CmdInit <$> parseInit)
        <|> (CmdUpload <$> parsePublish)
        <|> subparser (command "reg" (info reg $ progDesc "Manage configured registries") <> metavar "reg")
        <|> parseIndex
        <|> parseDeindex
        <|> (CmdListUnindexed <$> parseListUnindexed)
        <|> parseCat
        <|> parsePkgCat
        <|> (CmdEosUpload <$> parseEosPublish)
    where
        reg = parseRepoAdd <|> (CmdRegDel <$> parseRepoDel) <|> (parseRepoList $> CmdRegList)


parseCat :: Parser Command
parseCat = subparser $ command "category" (info (add <|> del) $ progDesc "Manage categories")
    where
        add =
            subparser $
                command
                    "add"
                    ( info
                        ( CmdCatAdd
                            <$> strOption (short 't' <> long "target" <> metavar "REGISTRY_NAME")
                            <*> strArgument (metavar "CATEGORY")
                            <*> optional (strOption (short 'd' <> long "description" <> metavar "DESCRIPTION"))
                            <*> optional
                                (option Options.Applicative.auto (short 'p' <> long "priority" <> metavar "PRIORITY"))
                        )
                        $ progDesc "Adds category to registry"
                    )
        del =
            subparser $
                command
                    "rm"
                    ( info
                        ( CmdCatDel <$> strOption (short 't' <> long "target" <> metavar "REGISTRY_NAME")
                            <*> strArgument
                                (metavar "CATEGORY")
                        )
                        $ progDesc "Removes category from registry"
                    )


parsePkgCat :: Parser Command
parsePkgCat = subparser $ command "categorize" (info cat $ progDesc "Add or remove package from category")
    where
        cat :: Parser Command
        cat =
            let cmd rm = if not rm then CmdPkgCatAdd else CmdPkgCatDel
             in cmd
                    <$> switch (long "remove")
                    <*> strOption (short 't' <> long "target" <> metavar "REGISTRY_NAME")
                    <*> strArgument (metavar "PACKAGE_ID")
                    <*> strArgument (metavar "CATEGORY")


parseEosPublish :: Parser EosUpload
parseEosPublish =
    subparser $
        command "eos-upload" (info go $ progDesc "Publishes a .img to a remote registry")
            <> metavar
                "eos-upload"
    where
        go =
            liftA3
                EosUpload
                (strOption (short 't' <> long "target" <> metavar "NAME" <> help "Name of registry in publish.dhall"))
                (strOption (short 'i' <> long "image" <> metavar "EOS_IMG" <> help "File path of the image to publish"))
                (strOption (short 'v' <> long "version" <> help "Version of the image"))


opts :: ParserInfo Command
opts = info (parseCommand <**> helper) (fullDesc <> progDesc "Publish tool for Embassy Packages")


cliMain :: IO ()
cliMain =
    execParser opts >>= \case
        CmdInit sh -> init sh
        CmdRegAdd s pcr -> regAdd s pcr
        CmdRegDel s -> regRm s
        CmdRegList -> regLs
        CmdUpload up -> upload up
        CmdIndex name pkg v shouldIndex -> if shouldIndex then index name pkg v else deindex name pkg v
        CmdListUnindexed name -> listUnindexed name
        CmdCatAdd target cat desc pri -> catAdd target cat desc pri
        CmdCatDel target cat -> catDel target cat
        CmdPkgCatAdd target pkg cat -> pkgCatAdd target pkg cat
        CmdPkgCatDel target pkg cat -> pkgCatDel target pkg cat
        CmdEosUpload up -> eosUpload up


init :: Maybe Shell -> IO ()
init sh = do
    loc <- cfgLocation
    createDirectoryIfMissing True (takeDirectory loc)
    unlessM (doesPathExist loc) $ do
        writeFile loc (pretty $ embed inject (def @PublishCfg))
        home <- getHomeDirectory
        for_ sh $ \case
            Bash -> do
                let bashrc = home </> ".bashrc"
                appendFile bashrc "source <(embassy-publish --bash-completion-script `which embassy-publish`)\n"
            Fish -> do
                let fishrc = home </> ".config" </> "fish" </> "config.fish"
                appendFile fishrc "source <(embassy-publish --fish-completion-script `which embassy-publish`)\n"
            Zsh -> do
                let zshcompleter = "/usr/local/share/zsh/site-functions/_embassy-publish"
                res <- readProcess "embassy-publish" ["--zsh-completion-script", "`which embassy-publish`"] ""
                writeFile zshcompleter (toS res)


regAdd :: String -> PublishCfgRepo -> IO ()
regAdd name val = do
    loc <- cfgLocation
    PublishCfg cfg <- inputFile Dhall.auto loc
    let cfg' = insert name val cfg
    writeFile loc (pretty $ embed inject $ PublishCfg cfg')
    putChunkLn $ "Below is the hash to provide to the server operator for your admin credentials" & fore yellow
    putChunkLn
        . fore yellow
        . chunk
        . decodeUtf8
        . convertToBase Base16
        . hashWith SHA256
        . B8.pack
        . mappend "start9_admin:"
        $ publishCfgRepoPass val


regRm :: String -> IO ()
regRm name = do
    loc <- cfgLocation
    PublishCfg cfg <- inputFile Dhall.auto loc
    let cfg' = delete name cfg
    writeFile loc (pretty $ embed inject $ PublishCfg cfg')


regLs :: IO ()
regLs = do
    loc <- cfgLocation
    PublishCfg cfg <- inputFile Dhall.auto loc
    void $ traverseWithKey f cfg
    where
        f k v = do
            putChunk $ fromString (k <> ": ") & fore yellow
            putChunkLn $ fromString (show $ publishCfgRepoLocation v) & fore magenta


upload :: Upload -> IO ()
upload (Upload name mpkg shouldIndex) = do
    PublishCfgRepo{..} <- findNameInCfg name
    pkg <- case mpkg of
        Nothing -> do
            cwd <- getCurrentDirectory
            files <- listDirectory cwd
            let pkgs = filter (\n -> takeExtension n == ".s9pk") files
            case pkgs of
                [] -> do
                    $logError "No package specified, and could not find one in this directory"
                    exitWith $ ExitFailure 1
                [p] -> pure (cwd </> p)
                (_ : _ : _) -> do
                    $logWarn "Ambiguous package upload request, found multiple candidates:"
                    for_ pkgs $ \f -> $logWarn (fromString f)
                    exitWith $ ExitFailure 1
        Just s -> pure s
    noBody <-
        parseRequest ("POST " <> show publishCfgRepoLocation <> "/admin/v0/upload")
            <&> setRequestHeaders [("accept", "text/plain")]
            <&> applyBasicAuth (B8.pack publishCfgRepoUser) (B8.pack publishCfgRepoPass)
    size <- getFileSize pkg
    bar <- newProgressBar defStyle 30 (Progress 0 (fromIntegral size) ())
    body <- observedStreamFile (updateProgress bar . const . sfs2prog) pkg
    let withBody = setRequestBody body noBody
    manager <- newTlsManager
    res <- runReaderT (httpLbs withBody) manager
    if getResponseStatus res == status200
        then -- no output is successful
            pure ()
        else do
            $logError (decodeUtf8 . LB.toStrict $ getResponseBody res)
            exitWith $ ExitFailure 1
    putChunkLn $ fromString ("Successfully uploaded " <> pkg) & fore green
    when shouldIndex $ do
        home <- getHomeDirectory
        manifestBytes <- sourceManifest (home </> ".cargo/bin") pkg $ \c -> runConduit (c .| foldC)
        PackageManifest{packageManifestId, packageManifestVersion} <- case eitherDecodeStrict manifestBytes of
            Left s -> do
                $logError $ "Could not parse the manifest of the package: " <> toS s
                exitWith $ ExitFailure 1
            Right a -> pure a
        let pkgId = toS $ unPkgId packageManifestId
        index name pkgId packageManifestVersion
        putChunkLn $ fromString ("Successfully indexed " <> pkgId <> "@" <> show packageManifestVersion) & fore green
    where
        sfs2prog :: StreamFileStatus -> Progress ()
        sfs2prog StreamFileStatus{..} = Progress (fromIntegral readSoFar) (fromIntegral fileSize) ()


eosUpload :: EosUpload -> IO ()
eosUpload (EosUpload name img version) = do
    PublishCfgRepo{..} <- findNameInCfg name
    size <- fromInteger <$> getFileSize img
    noBody <-
        parseRequest ("POST " <> show publishCfgRepoLocation <> "/admin/v0/eos-upload")
            <&> setRequestHeaders [("accept", "text/plain")]
            <&> setRequestHeaders [("Content-Encoding", "gzip")]
            <&> setRequestHeaders [("Content-Length", show size)]
            <&> applyBasicAuth (B8.pack publishCfgRepoUser) (B8.pack publishCfgRepoPass)
    hash <- hashFile @_ @SHA256 img
    bar <- newProgressBar defStyle 30 (Progress 0 (fromIntegral size) ())
    body <- observedStreamFile (updateProgress bar . const . sfs2prog) img
    let withBody = setRequestBody body noBody
    let withQParams =
            setRequestQueryString
                [("version", Just $ show version), ("hash", Just $ convertToBase Base16 hash)]
                withBody
    manager <- newTlsManager
    res <- runReaderT (httpLbs withQParams) manager
    if getResponseStatus res == status200
        then -- no output is successful
            pure ()
        else do
            $logError (decodeUtf8 . LB.toStrict $ getResponseBody res)
            exitWith $ ExitFailure 1
    putChunkLn $ fromString ("Successfully uploaded " <> img) & fore green
    where
        sfs2prog :: StreamFileStatus -> Progress ()
        sfs2prog StreamFileStatus{..} = Progress (fromIntegral readSoFar) (fromIntegral fileSize) ()


index :: String -> String -> Version -> IO ()
index name pkg v = performHttp name "POST" [i|/admin/v0/index|] (IndexPkgReq (PkgId $ toS pkg) v)


deindex :: String -> String -> Version -> IO ()
deindex name pkg v = performHttp name "POST" [i|/admin/v0/deindex|] (IndexPkgReq (PkgId $ toS pkg) v)


listUnindexed :: String -> IO ()
listUnindexed name = do
    PublishCfgRepo{..} <- findNameInCfg name
    noBody <-
        parseRequest (show publishCfgRepoLocation <> "/admin/v0/deindex")
            <&> setRequestHeaders [("accept", "application/json")]
            <&> applyBasicAuth (B8.pack publishCfgRepoUser) (B8.pack publishCfgRepoPass)
    PackageList{..} <- getResponseBody <$> httpJSON noBody
    void $
        flip traverseWithKey unPackageList $ \k v -> do
            putChunk (chunk (unPkgId k <> ": ") & fore blue)
            putChunkLn $ chunk (show v) & fore yellow


catAdd :: String -> String -> Maybe String -> Maybe Int -> IO ()
catAdd target name desc pri =
    performHttp target "POST" [i|/admin/v0/category/#{name}|] (AddCategoryReq (toS <$> desc) pri)


catDel :: String -> String -> IO ()
catDel target name = performHttp target "DELETE" [i|/admin/v0/category/#{name}|] ()


pkgCatAdd :: String -> PkgId -> String -> IO ()
pkgCatAdd target pkg cat = performHttp target "POST" [i|/admin/v0/categorize/#{cat}/#{pkg}|] ()


pkgCatDel :: String -> PkgId -> String -> IO ()
pkgCatDel target pkg cat = performHttp target "DELETE" [i|/admin/v0/categorize/#{cat}/#{pkg}|] ()


findNameInCfg :: String -> IO PublishCfgRepo
findNameInCfg name = do
    loc <- cfgLocation
    PublishCfg cfg <- inputFile Dhall.auto loc
    case lookup name cfg of
        Nothing -> do
            $logError "Registry name not found!"
            exitWith $ ExitFailure 1
        Just pcr -> pure pcr


performHttp :: ToJSON a => String -> String -> String -> a -> IO ()
performHttp target method route body = do
    PublishCfgRepo{..} <- findNameInCfg target
    noBody <-
        parseRequest (method <> " " <> show publishCfgRepoLocation <> route)
            <&> setRequestHeaders [("accept", "text/plain")]
            <&> applyBasicAuth (B8.pack publishCfgRepoUser) (B8.pack publishCfgRepoPass)
    let withBody = setRequestBodyJSON body noBody
    res <- httpLBS withBody
    if getResponseStatus res == status200
        then pure ()
        else do
            $logError (decodeUtf8 . LB.toStrict $ getResponseBody res)
            exitWith (ExitFailure 1)


instance MonadLogger IO where
    monadLoggerLog _ _ LevelDebug = putChunkLn . colorLog white
    monadLoggerLog _ _ LevelInfo = putChunkLn . colorLog blue
    monadLoggerLog _ _ LevelWarn = putChunkLn . colorLog yellow
    monadLoggerLog _ _ LevelError = putChunkLn . colorLog red
    monadLoggerLog _ _ (LevelOther _) = putChunkLn . colorLog magenta


colorLog :: ToLogStr msg => Radiant -> msg -> Chunk
colorLog c m = fore c $ chunk . decodeUtf8 . fromLogStr . toLogStr $ m
instance MonadLoggerIO IO where
    askLoggerIO = pure monadLoggerLog
