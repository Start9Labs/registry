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

------------------------------------------------------------------------------------------------------------------------
-- Semver AppVersion
------------------------------------------------------------------------------------------------------------------------

newtype AppVersion = AppVersion
    { unAppVersion :: (Word16, Word16, Word16, Word16) } deriving (Eq, Ord)

instance Read AppVersion where
    readsPrec _ s = case traverse (readMaybe . toS) $ splitOn "+" <=< splitOn "." $ (toS s) of
        Just [major, minor, patch, build] -> [(AppVersion (major, minor, patch, build), "")]
        Just [major, minor, patch]        -> [(AppVersion (major, minor, patch,     0), "")]
        _                                 -> []
instance PathPiece AppVersion where
    fromPathPiece = readMaybe . toS
    toPathPiece = show

instance Show AppVersion where
    show (AppVersion (a, b, c, d))
        | d == 0 = [i|#{a}.#{b}.#{c}|]
        | otherwise = [i|#{a}.#{b}.#{c}+#{d}|]

instance IsString AppVersion where
    fromString s = case traverse (readMaybe . toS) $ splitOn "+" <=< splitOn "." $ (toS s) of
        Just [major, minor, patch, build] -> AppVersion (major, minor, patch, build)
        Just [major, minor, patch]        -> AppVersion (major, minor, patch, 0)
        _                                 -> panic . toS $ "Invalid App Version: " <> s
instance ToJSON AppVersion where
    toJSON = String . show
instance FromJSON AppVersion where
    parseJSON = withText "app version" $ \t ->
        case traverse (decode . toS) $ splitOn "+" <=< splitOn "." $ t  of
            Just [a, b, c, d] -> pure $ AppVersion (a, b, c, d)
            Just [a, b, c]    -> pure $ AppVersion (a, b, c, 0)
            _                 -> fail "unknown versioning"
instance ToTypedContent AppVersion where
    toTypedContent = toTypedContent . toJSON
instance ToContent AppVersion where
    toContent = toContent . toJSON

------------------------------------------------------------------------------------------------------------------------
-- Semver AppVersionSpecification
------------------------------------------------------------------------------------------------------------------------

data AppVersionSpecification =
      AppVersionAny
    | AppVersionSpecification SemverRequestModifier AppVersion

instance Read AppVersionSpecification where
    readsPrec _ s =
        if s == "*"
            then [(AppVersionAny, "")]
            else case (readMaybe . toS $ svMod, readMaybe . toS $ version) of
                (Just m, Just av) -> [(AppVersionSpecification m av, "")]
                _                 -> []
        where
            (svMod, version) = break isDigit . toS $ s

instance PathPiece AppVersionSpecification where
    fromPathPiece = readMaybe . toS
    toPathPiece = show

instance Show AppVersionSpecification where
    show AppVersionAny                 = "*"
    show (AppVersionSpecification r b) = show r <> show b
instance ToJSON AppVersionSpecification where
    toJSON = String . show
instance FromJSON AppVersionSpecification where
    parseJSON = withText "app version spec" $ \t ->
        if t == "*"
            then pure AppVersionAny
            else do
                let (svMod, version) = break isDigit t
                baseVersion <- parseJSON . String $ version
                requestModifier <- parseJSON . String $ svMod
                pure $ AppVersionSpecification requestModifier baseVersion

mostRecentVersion :: AppVersionSpecification
mostRecentVersion = AppVersionSpecification SVGreaterThanEq $ AppVersion (0,0,0,0)

------------------------------------------------------------------------------------------------------------------------
-- Semver RequestModifier
------------------------------------------------------------------------------------------------------------------------

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
    parseJSON = withText "semver request modifier" $ \t ->
        case readMaybe . toS $ t of
            Just m  -> pure m
            Nothing -> fail "invalid semver request modifier"

instance Read SemverRequestModifier where
    readsPrec _ = \case
        ""   -> [(SVGreatestWithMajorMinor, "")]
        "="  -> [(SVEquals, "")]
        "<"  -> [(SVLessThan, "")]
        ">"  -> [(SVGreaterThan, "")]
        "~"  -> [(SVGreatestWithMajor, "")]
        "^"  -> [(SVGreatestWithMajorMinor, "")]
        "<=" -> [(SVLessThanEq, "")]
        ">=" -> [(SVGreaterThanEq, "")]
        _    -> []
