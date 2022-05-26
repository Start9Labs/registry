{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE RecordWildCards #-}

module Cli.Cli
    ( cliMain
    ) where

import           Conduit                        ( (.|)
                                                , foldC
                                                , runConduit
                                                )
import           Control.Monad.Logger           ( LogLevel(..)
                                                , MonadLogger(monadLoggerLog)
                                                , MonadLoggerIO(askLoggerIO)
                                                , ToLogStr
                                                , fromLogStr
                                                , toLogStr
                                                )
import           Crypto.Hash                    ( SHA256(SHA256)
                                                , hashWith
                                                )
import           Data.Aeson                     ( eitherDecodeStrict )
import           Data.ByteArray.Encoding        ( Base(..)
                                                , convertToBase
                                                )
import qualified Data.ByteString.Char8         as B8
import qualified Data.ByteString.Lazy          as LB
import           Data.Default
import           Data.Functor.Contravariant     ( contramap )
import           Data.HashMap.Internal.Strict   ( HashMap
                                                , delete
                                                , empty
                                                , insert
                                                , lookup
                                                , traverseWithKey
                                                )
import           Dhall                   hiding ( void )
import           Dhall.Core                     ( pretty )
import           Handler.Admin                  ( IndexPkgReq(IndexPkgReq) )
import           Lib.External.AppMgr            ( sourceManifest )
import           Lib.Types.AppIndex             ( PackageManifest
                                                    ( PackageManifest
                                                    , packageManifestId
                                                    , packageManifestVersion
                                                    )
                                                , PkgId(..)
                                                )
import           Lib.Types.Emver                ( Version(..) )
import           Network.HTTP.Client.Conduit    ( StreamFileStatus(StreamFileStatus, fileSize, readSoFar)
                                                , applyBasicAuth
                                                , httpLbs
                                                , observedStreamFile
                                                , parseRequest
                                                )
import           Network.HTTP.Client.TLS        ( newTlsManager )
import           Network.HTTP.Simple            ( getResponseBody
                                                , httpLBS
                                                , setRequestBody
                                                , setRequestBodyJSON
                                                , setRequestHeaders
                                                )
import           Network.URI                    ( URI
                                                , parseURI
                                                )
import           Options.Applicative     hiding ( auto
                                                , empty
                                                )
import           Rainbow                        ( Chunk
                                                , Radiant
                                                , blue
                                                , chunk
                                                , fore
                                                , green
                                                , magenta
                                                , putChunk
                                                , putChunkLn
                                                , red
                                                , white
                                                , yellow
                                                )
import           Startlude                      ( ($)
                                                , ($>)
                                                , (&)
                                                , (.)
                                                , (<&>)
                                                , Bool(..)
                                                , ConvertText(toS)
                                                , Either(..)
                                                , Eq(..)
                                                , ExitCode(..)
                                                , FilePath
                                                , IO
                                                , IsString(..)
                                                , Maybe(..)
                                                , Monad((>>=))
                                                , ReaderT(runReaderT)
                                                , Semigroup((<>))
                                                , Show
                                                , String
                                                , const
                                                , decodeUtf8
                                                , exitWith
                                                , filter
                                                , for_
                                                , fromIntegral
                                                , fromMaybe
                                                , panic
                                                , putStrLn
                                                , show
                                                , unlessM
                                                , void
                                                , when
                                                , writeFile
                                                )
import           System.Directory               ( createDirectoryIfMissing
                                                , doesPathExist
                                                , getCurrentDirectory
                                                , getFileSize
                                                , getHomeDirectory
                                                , listDirectory
                                                )
import           System.FilePath                ( (</>)
                                                , takeDirectory
                                                , takeExtension
                                                )
import           System.ProgressBar             ( Progress(..)
                                                , defStyle
                                                , newProgressBar
                                                , updateProgress
                                                )
import           Yesod                          ( logError
                                                , logWarn
                                                )

data Upload = Upload
    { publishRepoName :: String
    , publishPkg      :: Maybe FilePath
    , publishIndex    :: Bool
    }
    deriving Show

newtype PublishCfg = PublishCfg
    { publishCfgRepos :: HashMap String PublishCfgRepo
    }
    deriving Generic
instance FromDhall PublishCfg
instance ToDhall PublishCfg
instance Default PublishCfg where
    def = PublishCfg empty


data PublishCfgRepo = PublishCfgRepo
    { publishCfgRepoLocation :: URI
    , publishCfgRepoUser     :: String
    , publishCfgRepoPass     :: String
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

data Command
    = CmdInit
    | CmdRegAdd String PublishCfgRepo
    | CmdRegDel String
    | CmdRegList
    | CmdUpload Upload
    | CmdIndex String String Version Bool
    deriving Show

cfgLocation :: IO FilePath
cfgLocation = getHomeDirectory <&> \d -> d </> ".embassy/publish.dhall"

parseInit :: Parser ()
parseInit =
    subparser $ command "init" (info (pure ()) $ progDesc "Initializes embassy-publish config") <> metavar "init"

parsePublish :: Parser Upload
parsePublish = subparser $ command "upload" (info go $ progDesc "Publishes a .s9pk to a remote registry") <> metavar
    "upload"
    where
        go = liftA3
            Upload
            (strOption (short 't' <> long "target" <> metavar "NAME" <> help "Name of registry in publish.dhall"))
            (optional $ strOption
                (short 'p' <> long "package" <> metavar "S9PK" <> help "File path of the package to publish")
            )
            (switch (short 'i' <> long "index" <> help "Index the package after uploading"))

parseRepoAdd :: Parser Command
parseRepoAdd = subparser $ command "add" (info go $ progDesc "Add a registry to your config") <> metavar "add"
    where
        go :: Parser Command
        go =
            let
                publishCfgRepoLocation =
                    strOption (short 'l' <> long "location" <> metavar "REGISTRY_URL" <> help "Registry URL")
                publishCfgRepoUser = strOption
                    (short 'u' <> long "username" <> metavar "USERNAME" <> help "Admin username for this registry")
                publishCfgRepoPass = strOption
                    (short 'p' <> long "password" <> metavar "PASSWORD" <> help "Admin password for this registry")
                name =
                    strOption
                        (short 'n' <> long "name" <> metavar "REGISTRY_NAME" <> help
                            "Name to reference this registry in the future"
                        )
                r = PublishCfgRepo <$> publishCfgRepoLocation <*> publishCfgRepoUser <*> publishCfgRepoPass
            in
                liftA2 CmdRegAdd name r

parseRepoDel :: Parser String
parseRepoDel = subparser $ command "rm" (info go $ progDesc "Remove a registry from your config") <> metavar "rm"
    where
        go = strOption
            (short 'n' <> long "name" <> metavar "REGISTRY_NAME" <> help
                "Registry name chosen when this was originally configured"
            )

parseRepoList :: Parser ()
parseRepoList = subparser $ command "ls" (info (pure ()) $ progDesc "List registries in your config") <> metavar "ls"

parseIndex :: Parser Command
parseIndex =
    subparser
        $  command "index" (info (parseIndexHelper True) $ progDesc "Indexes an existing package version")
        <> metavar "index"

parseDeindex :: Parser Command
parseDeindex =
    subparser
        $  command "deindex" (info (parseIndexHelper False) $ progDesc "Deindexes an existing package version")
        <> metavar "deindex"

parseIndexHelper :: Bool -> Parser Command
parseIndexHelper b =
    CmdIndex
        <$> strOption (short 't' <> long "target" <> metavar "REGISTRY_NAME")
        <*> strArgument (metavar "PKG")
        <*> strArgument (metavar "VERSION")
        <*> pure b

parseCommand :: Parser Command
parseCommand =
    (parseInit $> CmdInit)
        <|> (CmdUpload <$> parsePublish)
        <|> subparser (command "reg" (info reg $ progDesc "Manage configured registries") <> metavar "reg")
        <|> parseIndex
        <|> parseDeindex
    where reg = parseRepoAdd <|> (CmdRegDel <$> parseRepoDel) <|> (parseRepoList $> CmdRegList)

opts :: ParserInfo Command
opts = info (parseCommand <**> helper) (fullDesc <> progDesc "Publish tool for Embassy Packages")

cliMain :: IO ()
cliMain =
    execParser opts
        >>= (\case
                CmdInit                         -> init
                CmdRegAdd s pcr                 -> regAdd s pcr
                CmdRegDel s                     -> regRm s
                CmdRegList                      -> regLs
                CmdUpload up                    -> upload up
                CmdIndex name pkg v shouldIndex -> if shouldIndex then index name pkg v else deindex name pkg v
            )

init :: IO ()
init = do
    loc <- cfgLocation
    createDirectoryIfMissing True (takeDirectory loc)
    unlessM (doesPathExist loc) $ writeFile loc (pretty $ embed inject (def @PublishCfg))

regAdd :: String -> PublishCfgRepo -> IO ()
regAdd name val = do
    loc            <- cfgLocation
    PublishCfg cfg <- inputFile auto loc
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
    loc            <- cfgLocation
    PublishCfg cfg <- inputFile auto loc
    let cfg' = delete name cfg
    writeFile loc (pretty $ embed inject $ PublishCfg cfg')

regLs :: IO ()
regLs = do
    loc            <- cfgLocation
    PublishCfg cfg <- inputFile auto loc
    void $ traverseWithKey f cfg
    where
        f k v = do
            putChunk $ fromString (k <> ": ") & fore yellow
            putChunkLn $ fromString (show $ publishCfgRepoLocation v) & fore magenta

upload :: Upload -> IO ()
upload (Upload name mpkg shouldIndex) = do
    PublishCfgRepo {..} <- findNameInCfg name
    pkg                 <- case mpkg of
        Nothing -> do
            cwd   <- getCurrentDirectory
            files <- listDirectory cwd
            let pkgs = filter (\n -> takeExtension n == ".s9pk") files
            case pkgs of
                [] -> do
                    $logError "No package specified, and could not find one in this directory"
                    exitWith $ ExitFailure 1
                [p        ] -> pure (cwd </> p)
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
    bar  <- newProgressBar defStyle 30 (Progress 0 (fromIntegral size) ())
    body <- observedStreamFile (updateProgress bar . const . sfs2prog) pkg
    let withBody = setRequestBody body noBody
    manager <- newTlsManager
    res     <- getResponseBody <$> runReaderT (httpLbs withBody) manager
    if LB.null res
        then pure ()
        else do
            $logError (decodeUtf8 $ LB.toStrict res)
            exitWith $ ExitFailure 1
    putChunkLn $ fromString ("Successfully uploaded " <> pkg) & fore green
    when shouldIndex $ do
        home          <- getHomeDirectory
        manifestBytes <- sourceManifest (home </> ".cargo/bin") pkg $ \c -> runConduit (c .| foldC)
        PackageManifest { packageManifestId, packageManifestVersion } <- case eitherDecodeStrict manifestBytes of
            Left s -> do
                $logError $ "Could not parse the manifest of the package: " <> toS s
                exitWith $ ExitFailure 1
            Right a -> pure a
        let pkgId = toS $ unPkgId packageManifestId
        index name pkgId packageManifestVersion
        putChunkLn $ fromString ("Successfully indexed " <> pkgId <> "@" <> show packageManifestVersion) & fore green

    where
        sfs2prog :: StreamFileStatus -> Progress ()
        sfs2prog StreamFileStatus {..} = Progress (fromIntegral readSoFar) (fromIntegral fileSize) ()

index :: String -> String -> Version -> IO ()
index name pkg v = do
    PublishCfgRepo {..} <- findNameInCfg name
    noBody              <-
        parseRequest ("POST " <> show publishCfgRepoLocation <> "/admin/v0/index")
        <&> setRequestHeaders [("accept", "text/plain")]
        <&> applyBasicAuth (B8.pack publishCfgRepoUser) (B8.pack publishCfgRepoPass)
    let withBody = setRequestBodyJSON (IndexPkgReq (PkgId $ toS pkg) v) noBody
    res <- getResponseBody <$> httpLBS withBody
    if LB.null res then pure () else $logError (decodeUtf8 $ LB.toStrict res) *> exitWith (ExitFailure 1)


deindex :: String -> String -> Version -> IO ()
deindex name pkg v = do
    PublishCfgRepo {..} <- findNameInCfg name
    noBody              <-
        parseRequest ("POST " <> show publishCfgRepoLocation <> "/admin/v0/deindex")
        <&> setRequestHeaders [("accept", "text/plain")]
        <&> applyBasicAuth (B8.pack publishCfgRepoUser) (B8.pack publishCfgRepoPass)
    let withBody = setRequestBodyJSON (IndexPkgReq (PkgId $ toS pkg) v) noBody
    res <- getResponseBody <$> httpLBS withBody
    if LB.null res then pure () else $logError (decodeUtf8 $ LB.toStrict res) *> exitWith (ExitFailure 1)

findNameInCfg :: String -> IO PublishCfgRepo
findNameInCfg name = do
    loc            <- cfgLocation
    PublishCfg cfg <- inputFile auto loc
    case lookup name cfg of
        Nothing -> do
            $logError "Registry name not found!"
            exitWith $ ExitFailure 1
        Just pcr -> pure pcr

instance MonadLogger IO where
    monadLoggerLog _ _ LevelDebug     = putChunkLn . colorLog white
    monadLoggerLog _ _ LevelInfo      = putChunkLn . colorLog blue
    monadLoggerLog _ _ LevelWarn      = putChunkLn . colorLog yellow
    monadLoggerLog _ _ LevelError     = putChunkLn . colorLog red
    monadLoggerLog _ _ (LevelOther _) = putChunkLn . colorLog magenta

colorLog :: ToLogStr msg => Radiant -> msg -> Chunk
colorLog c m = fore c $ chunk . decodeUtf8 . fromLogStr . toLogStr $ m
instance MonadLoggerIO IO where
    askLoggerIO = pure monadLoggerLog
