{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE RecordWildCards #-}

module Cli.Cli
    ( cliMain
    ) where

import           Data.Default
import           Data.Functor.Contravariant     ( contramap )
import           Data.HashMap.Internal.Strict   ( HashMap
                                                , delete
                                                , empty
                                                , insert
                                                , traverseWithKey
                                                )
import           Data.String                    ( IsString(fromString) )
import           Dhall                   hiding ( void )
import           Dhall.Core                     ( pretty )
import           Network.URI                    ( URI
                                                , parseURI
                                                )
import           Options.Applicative     hiding ( auto
                                                , empty
                                                )
import           Rainbow                        ( fore
                                                , magenta
                                                , putChunk
                                                , putChunkLn
                                                , yellow
                                                )
import           Startlude                      ( ($)
                                                , ($>)
                                                , (&)
                                                , (.)
                                                , (<$>)
                                                , (<&>)
                                                , (>>=)
                                                , Bool(..)
                                                , FilePath
                                                , IO
                                                , IsString
                                                , Maybe
                                                , Monad(return)
                                                , Semigroup((<>))
                                                , Show
                                                , String
                                                , fromMaybe
                                                , panic
                                                , print
                                                , pure
                                                , show
                                                , unlessM
                                                , void
                                                , writeFile
                                                )
import           System.Directory               ( createDirectory
                                                , createDirectoryIfMissing
                                                , doesPathExist
                                                , getHomeDirectory
                                                )
import           System.FilePath                ( (</>)
                                                , takeBaseName
                                                , takeDirectory
                                                )

data Upload = Upload
    { publishRepoName :: String
    , publishPkg      :: Maybe FilePath
    , publishIndex    :: Bool
    }
    deriving Show

data PublishCfg = PublishCfg
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

data RegAdd = RegAdd
    deriving Show
data RegDel = RegDel
    deriving Show

data Command
    = CmdInit
    | CmdRegAdd String PublishCfgRepo
    | CmdRegDel String
    | CmdRegList
    | CmdUpload Upload
    deriving Show

cfgLocation :: IO FilePath
cfgLocation = getHomeDirectory <&> \d -> d </> ".embassy/publish.dhall"

parseInit :: Parser ()
parseInit = subparser $ command "init" (info (pure ()) $ progDesc "Initializes embassy-publish config")

parsePublish :: Parser Upload
parsePublish = subparser $ command "upload" (info go $ progDesc "Publishes a .s9pk to a remote registry")
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

parseCommand :: Parser Command
parseCommand = (parseInit $> CmdInit) <|> (CmdUpload <$> parsePublish) <|> subparser
    (command "reg" (info reg $ progDesc "Manage configured registries"))
    where reg = parseRepoAdd <|> (CmdRegDel <$> parseRepoDel) <|> (parseRepoList $> CmdRegList)

opts :: ParserInfo Command
opts = info (parseCommand <**> helper) (fullDesc <> progDesc "Publish tool for Embassy Packages")

cliMain :: IO ()
cliMain =
    execParser opts
        >>= (\case
                CmdInit         -> init
                CmdRegAdd s pcr -> regAdd s pcr
                CmdRegDel s     -> regRm s
                CmdRegList      -> regLs
                CmdUpload up    -> regUpload up
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

regUpload :: Upload -> IO ()
regUpload = panic "unimplemented"
