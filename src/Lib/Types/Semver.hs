{-# LANGUAGE QuasiQuotes     #-}
{-# LANGUAGE RecordWildCards #-}
module Lib.Types.Semver where

import           Startlude               hiding (break)

import qualified GHC.Read                (Read (..))
import qualified GHC.Show                (Show (..))

import           Control.Monad.Fail
import           Data.Aeson
import           Data.Char               (isDigit)
import           Data.String.Interpolate
import           Data.Text
import           Yesod.Core

newtype AppVersion = AppVersion
    { unAppVersion :: (Word16, Word16, Word16) } deriving (Eq, Ord)

instance Read AppVersion where
    readsPrec _ s = case traverse (readMaybe . toS) $ split (=='.') (toS s) of
        Just [major, minor, patch] -> [(AppVersion (major, minor, patch), "")]
        _                          -> []
instance PathPiece AppVersion where
    fromPathPiece = readMaybe . toS
    toPathPiece = show

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

data SemverRequestModifier = SVEquals | SVLessThan | SVGreaterThan | SVGreatestWithMajor | SVGreatestWithMajorMinor | SVLessThanEq | SVGreaterThanEq deriving (Eq, Bounded, Enum)
instance Show SemverRequestModifier where
    show SVEquals                 = "="
    show SVLessThan               = "<"
    show SVGreaterThan            = ">"
    show SVGreatestWithMajor      = "~"
    show SVGreatestWithMajorMinor = "^"
    show SVLessThanEq             = "<="
    show SVGreaterThanEq          = ">="

instance FromJSON SemverRequestModifier where
    parseJSON = withText "semver request modifier" $ \case
        ""   -> pure SVGreatestWithMajorMinor
        "="  -> pure SVEquals
        "<"  -> pure SVLessThan
        ">"  -> pure SVGreaterThan
        "~"  -> pure SVGreatestWithMajor
        "^"  -> pure SVGreatestWithMajorMinor
        "<=" -> pure SVLessThanEq
        ">=" -> pure SVGreaterThanEq
        _    -> fail "invalid semver request modifier"
