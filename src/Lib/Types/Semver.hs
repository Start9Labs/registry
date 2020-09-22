{-# LANGUAGE QuasiQuotes     #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

module Lib.Types.Semver where

import           Startlude               hiding ( break )

import qualified GHC.Read                       ( Read(..) )
import qualified GHC.Show                       ( Show(..) )

import           Control.Monad.Fail
import           Data.Aeson
import qualified Data.Attoparsec.ByteString.Char8
                                               as AttoBS
import           Data.Char                      ( isDigit )
import           Data.String.Interpolate
import           Data.Text
import           Yesod.Core
import           Database.Persist.Sql

------------------------------------------------------------------------------------------------------------------------
-- Semver AppVersion
------------------------------------------------------------------------------------------------------------------------

newtype AppVersion = AppVersion
    { unAppVersion :: (Word16, Word16, Word16, Word16) } deriving (Eq, Ord, Hashable)

instance Read AppVersion where
    readsPrec _ s = case traverse (readMaybe . toS) $ splitOn "+" <=< splitOn "." $ (toS s) of
        Just [major, minor, patch, build] -> [(AppVersion (major, minor, patch, build), "")]
        Just [major, minor, patch] -> [(AppVersion (major, minor, patch, 0), "")]
        _ -> []
instance PathPiece AppVersion where
    fromPathPiece = readMaybe . toS
    toPathPiece   = show

instance Show AppVersion where
    show (AppVersion (a, b, c, d)) | d == 0    = [i|#{a}.#{b}.#{c}|]
                                   | otherwise = [i|#{a}.#{b}.#{c}+#{d}|]

instance IsString AppVersion where
    fromString s = case traverse (readMaybe . toS) $ splitOn "+" <=< splitOn "." $ (toS s) of
        Just [major, minor, patch, build] -> AppVersion (major, minor, patch, build)
        Just [major, minor, patch] -> AppVersion (major, minor, patch, 0)
        _ -> panic . toS $ "Invalid App Version: " <> s
instance ToJSON AppVersion where
    toJSON = String . show
instance FromJSON AppVersion where
    parseJSON = withText "app version" $ \t -> case traverse (decode . toS) $ splitOn "+" <=< splitOn "." $ t of
        Just [a, b, c, d] -> pure $ AppVersion (a, b, c, d)
        Just [a, b, c]    -> pure $ AppVersion (a, b, c, 0)
        _                 -> fail "unknown versioning"
instance ToTypedContent AppVersion where
    toTypedContent = toTypedContent . toJSON
instance ToContent AppVersion where
    toContent = toContent . toJSON

instance FromJSONKey AppVersion where
    fromJSONKey = FromJSONKeyTextParser $ \t -> case readMaybe (toS t) of
        Nothing -> fail "invalid app version"
        Just x  -> pure x

instance PersistField AppVersion where
    toPersistValue   = toPersistValue @Text . show
    fromPersistValue = note "invalid app version" . readMaybe <=< fromPersistValue

instance PersistFieldSql AppVersion where
    sqlType _ = SqlString

------------------------------------------------------------------------------------------------------------------------
-- Semver AppVersionSpec
------------------------------------------------------------------------------------------------------------------------

data AppVersionSpec =
      AppVersionAny
    | AppVersionSpec SemverRequestModifier AppVersion
    deriving Eq

instance Read AppVersionSpec where
    readsPrec _ s = if s == "*"
        then [(AppVersionAny, "")]
        else case (readMaybe . toS $ svMod, readMaybe . toS $ version) of
            (Just m, Just av) -> [(AppVersionSpec m av, "")]
            _                 -> []
        where (svMod, version) = break isDigit . toS $ s

instance PathPiece AppVersionSpec where
    fromPathPiece = readMaybe . toS
    toPathPiece   = show

instance Show AppVersionSpec where
    show AppVersionAny        = "*"
    show (AppVersionSpec r b) = show r <> show b
instance ToJSON AppVersionSpec where
    toJSON = String . show
instance FromJSON AppVersionSpec where
    parseJSON = withText "app version spec" $ \t -> if t == "*"
        then pure AppVersionAny
        else do
            let (svMod, version) = break isDigit t
            baseVersion     <- parseJSON . String $ version
            requestModifier <- parseJSON . String $ svMod
            pure $ AppVersionSpec requestModifier baseVersion

instance PersistField AppVersionSpec where
    toPersistValue = PersistText . show
    fromPersistValue (PersistText spec) = note ("Invalid Semver Requirement: " <> spec) . readMaybe $ spec
    fromPersistValue other              = Left $ "Persistent Type Mismatch. Expected 'PersistText _' got " <> show other

instance PersistFieldSql AppVersionSpec where
    sqlType _ = SqlString

mostRecentVersion :: AppVersionSpec
mostRecentVersion = AppVersionSpec SVGreaterThanEq $ AppVersion (0, 0, 0, 0)

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
    parseJSON = withText "semver request modifier" $ \t -> case readMaybe . toS $ t of
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

semverParserBS :: AttoBS.Parser AppVersion
semverParserBS = do
    major <- AttoBS.decimal <* AttoBS.char '.'
    minor <- AttoBS.decimal <* AttoBS.char '.'
    patch <- AttoBS.decimal
    build <- AttoBS.option 0 $ AttoBS.char '+' *> AttoBS.decimal
    pure $ AppVersion (major, minor, patch, build)
