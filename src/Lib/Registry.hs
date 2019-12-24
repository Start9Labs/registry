module Lib.Registry where

import           Startlude         hiding (empty, toList)

import           Data.HashMap.Lazy hiding (mapMaybe)
import qualified GHC.Read          (Read (..))
import qualified GHC.Show          (Show (..))
import           System.Directory
import           System.FilePath
import           Yesod.Core

import           Data.Text         (isSuffixOf)

import           Lib.Semver
import           Lib.Types.Semver
import           Util.Function

newtype S9PK = S9PK String deriving (Eq)
instance Show S9PK where
    show (S9PK t) = t <.> "s9pk"

instance Read S9PK where
    readsPrec _ s = [(S9PK . take (m - n) $ s, "") | toS s9pk `isSuffixOf` toS s]
        where
            m = length s
            s9pk = ".s9pk" :: String
            n = length s9pk

instance PathPiece S9PK where
    fromPathPiece = readMaybe . toS
    toPathPiece = show

appResourceDir :: FilePath
appResourceDir = "./resources/apps"

sysResourceDir :: FilePath
sysResourceDir = "./resources/sys"

appManifestPath :: FilePath
appManifestPath = appResourceDir </> "apps.yaml"

appManifestFile :: FilePath
appManifestFile = "apps.yml"

s9pkExt :: String -> FilePath
s9pkExt = show . S9PK

type Registry = HashMap String (HashMap AppVersion FilePath)

newtype RegisteredAppVersion = RegisteredAppVersion (AppVersion, FilePath)
instance HasAppVersion RegisteredAppVersion where
    version (RegisteredAppVersion (av, _)) = av

loadAppRegistry :: MonadIO m => m Registry
loadAppRegistry = loadRegistry appResourceDir

loadSysRegistry :: MonadIO m => m Registry
loadSysRegistry = loadRegistry sysResourceDir

loadRegistry :: MonadIO m => FilePath -> m Registry
loadRegistry rootDirectory = liftIO $ do
    appDirectories <- getSubDirectories rootDirectory
    foldM
        ( \registry appId -> do
            subdirs <- getSubDirectories (rootDirectory </> appId)
            let validVersions = mapMaybe readMaybe subdirs
            versionApps <- for validVersions $ \v ->
                getAppFileFromDir rootDirectory appId v
                    >>= \case
                        Nothing      -> pure Nothing
                        Just appFile -> pure . Just $ (v, rootDirectory </> appId </> show v </> appFile)
            pure $ insert appId (fromList . catMaybes $ versionApps) registry
        ) empty appDirectories
    where
        getSubDirectories path = listDirectory path >>= filterM (fmap not . doesFileExist)


getAppFileFromDir :: String -> String -> AppVersion -> IO (Maybe FilePath)
getAppFileFromDir rootDirectory appId v = do
    dirContents <- listDirectory (rootDirectory </> appId </> show v)
    pure $ find (isPrefixOf appId) dirContents

getAppFile :: String -> Registry -> AppVersion -> Maybe FilePath
getAppFile appId r av = lookup av <=< lookup appId $ r

registeredAppVersions :: String -> Registry -> [RegisteredAppVersion]
registeredAppVersions appId r = maybe [] (fmap RegisteredAppVersion . toList) (lookup appId r)

findM :: Monad m => (a -> m Bool) -> [a] -> m (Maybe a)
findM = fmap headMay .* filterM
