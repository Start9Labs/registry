{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

module Lib.Registry where

import           Startlude

import qualified Data.Attoparsec.Text          as Atto
import           Data.HashMap.Lazy       hiding ( mapMaybe )
import qualified GHC.Read                       ( Read(..) )
import qualified GHC.Show                       ( Show(..) )
import           System.Directory
import           System.FilePath
import           Yesod.Core

import           Lib.Types.Emver

type Registry = HashMap String (HashMap Version FilePath)

newtype RegisteredAppVersion = RegisteredAppVersion { unRegisteredAppVersion :: (Version, FilePath) } deriving (Eq, Show)
data MaxVersion a = MaxVersion
    { getMaxVersion :: (a, a -> Version)
    }
instance Semigroup (MaxVersion a) where
    (MaxVersion (a, f)) <> (MaxVersion (b, g)) = if f a > g b then MaxVersion (a, f) else MaxVersion (b, g)

-- retrieve all valid semver folder names with queried for file: rootDirectory/appId/[0.0.0 ...]/appId.extension
-- TODO move to db query after all appversions are seeded qith post 0.3.0 migration script
getAvailableAppVersions :: KnownSymbol a => FilePath -> Extension a -> IO [RegisteredAppVersion]
getAvailableAppVersions rootDirectory ext@(Extension appId) = do
    versions <- mapMaybe (hush . Atto.parseOnly parseVersion . toS) <$> getSubDirectories (rootDirectory </> appId)
    fmap catMaybes . for versions $ \v -> getVersionedFileFromDir rootDirectory ext v >>= \case
        Nothing      -> pure Nothing
        Just appFile -> pure . Just $ RegisteredAppVersion (v, appFile)
    where
        getSubDirectories path = (fmap (fromRight []) . try @SomeException $ listDirectory path)
            >>= filterM (doesDirectoryExist . (path </>))

-- this works for both service versions and embassyOS versions
getMostRecentAppVersion :: KnownSymbol a => FilePath -> Extension a -> IO (Maybe RegisteredAppVersion)
getMostRecentAppVersion rootDirectory ext = do
    allVersions <- liftIO $ getAvailableAppVersions rootDirectory ext
    pure $ head $ sortOn (Down . fst . unRegisteredAppVersion) allVersions

-- /root/appId/version/appId.ext
getVersionedFileFromDir :: KnownSymbol a => FilePath -> Extension a -> Version -> IO (Maybe FilePath)
getVersionedFileFromDir rootDirectory ext@(Extension appId) v =
    getUnversionedFileFromDir (rootDirectory </> appId </> show v) ext

-- /root/appId.ext
getUnversionedFileFromDir :: KnownSymbol a => FilePath -> Extension a -> IO (Maybe FilePath)
getUnversionedFileFromDir rootDirectory appExt = fmap (join . hush) . try @SomeException $ do
    dirContents <- listDirectory rootDirectory
    pure . fmap (rootDirectory </>) $ find (== show appExt) dirContents

newtype Extension (a :: Symbol) = Extension String deriving (Eq)
type S9PK = Extension "s9pk"
type SYS_EXTENSIONLESS = Extension ""
type PNG = Extension "png"
type SVG = Extension "svg"

instance IsString (Extension a) where
    fromString = Extension

def :: Extension a
def = Extension ""

extension :: KnownSymbol a => Extension a -> String
extension = symbolVal

instance KnownSymbol a => Show (Extension a) where
    show e@(Extension file) = file <.> extension e

instance KnownSymbol a => Read (Extension a) where
    readsPrec _ s = case symbolVal $ Proxy @a of
        ""    -> [(Extension s, "")]
        other -> [ (Extension file, "") | ext' == "" <.> other ]
        where (file, ext') = splitExtension s

withPeriod :: String -> String
withPeriod word@(a : _) = case a of
    '.' -> word
    _   -> "." <> word
withPeriod word = word

instance KnownSymbol a => PathPiece (Extension a) where
    fromPathPiece = readMaybe . toS
    toPathPiece   = show
