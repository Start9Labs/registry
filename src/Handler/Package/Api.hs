{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Handler.Package.Api where

import Data.ByteString.Base64 (encodeBase64)
import Data.HashMap.Strict
import Data.String.Interpolate.IsString (i)
import Handler.Types.Api (ApiResponse (..), ApiVersion (..))
import Lib.Types.Core
import Lib.Types.Emver
import Startlude (
    ByteString,
    Eq,
    Generic,
    NonEmpty,
    Show,
    Text,
    snd,
    ($),
    (&),
    (.),
    (<$>),
    UTCTime,
 )
import Yesod
import Data.Time.Format.ISO8601 (iso8601Show)


dataUrl :: (ContentType, ByteString) -> Text
dataUrl (ct, payload) = [i|data:#{ct};base64,#{encodeBase64 payload}|]


newtype PackageListRes = PackageListRes [PackageRes]
    deriving (Generic)
instance ApiResponse PackageListRes where
    apiEncode V0 (PackageListRes pkgs) = toJSON $ apiEncode V0 <$> pkgs
    apiEncode V1 (PackageListRes pkgs) = toJSON $ apiEncode V1 <$> pkgs


data PackageRes = PackageRes
    { packageResIcon :: !(ContentType, ByteString)
    , packageResManifest :: !Value -- PackageManifest
    , packageResCategories :: ![Text]
    , packageResInstructions :: !Text
    , packageResLicense :: !Text
    , packageResVersions :: !(NonEmpty Version)
    , packageResDependencies :: !(HashMap PkgId DependencyRes)
    , packageResPublishedAt :: !UTCTime
    }
    deriving (Show, Generic)


instance ApiResponse PackageRes where
    apiEncode v PackageRes{..} =
        object
            [ "icon"
                .= ( packageResIcon & case v of
                        V0 -> encodeBase64 . snd
                        V1 -> dataUrl
                   )
            , "license" .= packageResLicense
            , "instructions" .= packageResInstructions
            , "manifest" .= packageResManifest
            , "categories" .= packageResCategories
            , "versions" .= packageResVersions
            , "dependency-metadata" .= (apiEncode v <$> packageResDependencies)
            , "published-at" .= (iso8601Show packageResPublishedAt)
            ]


data DependencyRes = DependencyRes
    { dependencyResTitle :: !Text
    , dependencyResIcon :: !(ContentType, ByteString)
    }
    deriving (Eq, Show)


instance ApiResponse DependencyRes where
    apiEncode V0 DependencyRes{..} = object ["icon" .= encodeBase64 (snd dependencyResIcon), "title" .= dependencyResTitle]
    apiEncode V1 DependencyRes{..} = object ["icon" .= dataUrl dependencyResIcon, "title" .= dependencyResTitle]
