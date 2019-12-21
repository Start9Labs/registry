{-# LANGUAGE QuasiQuotes     #-}
{-# LANGUAGE RecordWildCards #-}
module Lib.Types.ServerApp where

import           Startlude               hiding (break)

import qualified GHC.Show                (Show (..))

import           Control.Monad.Fail
import           Data.Aeson
import           Data.Char               (isDigit)
import           Data.String.Interpolate
import           Data.Text
import           Yesod.Core

data StoreApp = StoreApp
    { storeAppId               :: Text
    , storeAppTitle            :: Text
    , storeAppDescriptionShort :: Text
    , storeAppDescriptionLong  :: Text
    , storeAppIconUrl          :: Text
    , storeAppVersions         :: NonEmpty StoreAppVersionInfo
    } deriving (Eq, Show)

data StoreAppVersionInfo = StoreAppVersionInfo
    { storeAppVersionInfoVersion      :: AppVersion
    , storeAppVersionInfoReleaseNotes :: Text
    } deriving (Eq, Ord, Show)
instance FromJSON StoreAppVersionInfo where
    parseJSON = withObject "Store App Version Info" $ \o -> do
        storeAppVersionInfoVersion <- o .: "version"
        storeAppVersionInfoReleaseNotes <- o .: "release-notes"
        pure StoreAppVersionInfo{..}
instance ToJSON StoreAppVersionInfo where
    toJSON StoreAppVersionInfo{..} = object
        [ "version" .= storeAppVersionInfoVersion
        , "releaseNotes" .= storeAppVersionInfoReleaseNotes
        ]

data ServerApp = ServerApp
    { serverAppId               :: Text
    , serverAppVersionInstalled :: AppVersion
    , serverAppTorService       :: Text
    , serverAppIsConfigured     :: Bool
    } deriving (Eq, Show)

data SemverRequestModifier = SVEquals | SVLessThan | SVGreaterThan | SVMinMinor | SVMinPatch | SVLessThanEq | SVGreaterThanEq deriving (Eq, Bounded, Enum)
instance Show SemverRequestModifier where
    show SVEquals        = "="
    show SVLessThan      = "<"
    show SVGreaterThan   = ">"
    show SVMinMinor      = "~"
    show SVMinPatch      = "^"
    show SVLessThanEq    = "<="
    show SVGreaterThanEq = ">="

instance FromJSON SemverRequestModifier where
    parseJSON = withText "semver request modifier" $ \case
        ""   -> pure SVMinPatch
        "="  -> pure SVEquals
        "<"  -> pure SVLessThan
        ">"  -> pure SVGreaterThan
        "~"  -> pure SVMinMinor
        "^"  -> pure SVMinPatch
        "<=" -> pure SVLessThanEq
        ">=" -> pure SVGreaterThanEq
        _    -> fail "invalid semver request modifier"

data AppVersionSpecification = AppVersionSpecification
    { requestModifier :: SemverRequestModifier
    , baseVersion     :: AppVersion
    }

instance Show AppVersionSpecification where
    show (AppVersionSpecification r b) = show r <> show b
instance ToJSON AppVersionSpecification where
    toJSON = String . show
instance FromJSON AppVersionSpecification where
    parseJSON = withText "app version spec" $ \t -> do
        let (svMod, version) = break isDigit t
        baseVersion <- parseJSON . String $ version
        requestModifier <- parseJSON . String $ svMod
        pure $ AppVersionSpecification {..}

(<||) :: AppVersion -> AppVersionSpecification -> Bool
(<||) av (AppVersionSpecification SVEquals av1)        = av == av1
(<||) av (AppVersionSpecification SVLessThan av1)      = av < av1
(<||) av (AppVersionSpecification SVGreaterThan av1)   = av > av1
(<||) av (AppVersionSpecification SVLessThanEq av1)    = av <= av1
(<||) av (AppVersionSpecification SVGreaterThanEq av1) = av >= av1
(<||) (AppVersion (a,b,_)) (AppVersionSpecification SVMinMinor (AppVersion (a1, b1, _)))
    = a == a1 && b >= b1
(<||) (AppVersion (a,b,c)) (AppVersionSpecification SVMinPatch (AppVersion (a1, b1, c1)))
    = a == a1 && b == b1 && c >= c1


newtype AppVersion = AppVersion
    { unAppVersion :: (Word16, Word16, Word16) } deriving (Eq, Ord)
instance Show AppVersion where
    show (AppVersion (a, b, c)) = [i|#{a}.#{b}.#{c}|]
instance IsString AppVersion where
    fromString s = case traverse (readMaybe . toS) $ split (=='.') (toS s) of
        Just [major, minor, patch] -> AppVersion (major, minor, patch)
        _                          -> panic . toS $ "Invalid App Version: " <> s
instance ToJSON AppVersion where
    toJSON av = String . toS $ let (a,b,c) = unAppVersion av in [i|#{a}.#{b}.#{c}|]
instance FromJSON AppVersion where
    parseJSON = withText "app version" $ \t ->
        case splitOn "." t of
            [a, b, c] ->
                case traverse (decode . toS) [a, b, c] of
                    Just [a', b', c'] -> pure $ AppVersion (a', b', c')
                    _                 -> fail "non word16 versioning"
            _ -> fail "unknown versioning"
instance ToTypedContent AppVersion where
    toTypedContent = toTypedContent . toJSON
instance ToContent AppVersion where
    toContent = toContent . toJSON

(\\) :: AppVersion -> AppVersion -> (Word16, Word16, Word16)
(\\) (AppVersion (a, b, c)) (AppVersion (a1, b1, c1)) = (a `diffy` a1, b `diffy` b1, c `diffy` c1)
    where
        d `diffy` d1 = fromIntegral . abs $ (fromIntegral d :: Integer) - (fromIntegral d1 :: Integer)

data AppStatus = Running | Stopped | Restarting | Removing | Dead deriving (Eq, Show)
instance ToJSON AppStatus where
    toJSON = String . toUpper . show
instance FromJSON AppStatus where
    parseJSON = withText "health status" $ \case
        "RUNNING" -> pure Running
        "STOPPED" -> pure Stopped
        "RESTARTING" -> pure Restarting
        "REMOVING" -> pure Removing
        "DEAD" -> pure Dead
        _         -> fail "unknown status"

data AppAction = Start | Stop deriving (Eq, Show)
