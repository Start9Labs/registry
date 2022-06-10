{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Handler.Package.Api where

import Data.ByteString.Base64 (encodeBase64)
import Data.HashMap.Strict
import Data.Singletons (TyCon)
import Data.Singletons.Sigma (Sigma (..))
import Data.String.Interpolate.IsString (i)
import Handler.Types.Api (ApiVersion (..), SApiVersion (..))
import Lib.Types.Core
import Lib.Types.Emver
import Startlude
import Yesod


dataUrl :: (ContentType, ByteString) -> Text
dataUrl (ct, payload) = [i|data:#{ct};base64,#{encodeBase64 payload}|]


type PackageListRes :: ApiVersion -> Type
newtype PackageListRes a = PackageListRes [PackageRes a]
    deriving (Generic)
instance ToJSON (PackageRes a) => ToJSON (PackageListRes a) where
    toJSON (PackageListRes a) = toJSON a
instance ToJSON (PackageRes a) => ToContent (PackageListRes a) where
    toContent = toContent . toJSON
instance ToJSON (PackageRes a) => ToTypedContent (PackageListRes a) where
    toTypedContent = toTypedContent . toJSON


data PackageRes a = PackageRes
    { packageResIcon :: !(ContentType, ByteString)
    , packageResManifest :: !Value -- PackageManifest
    , packageResCategories :: ![Text]
    , packageResInstructions :: !Text
    , packageResLicense :: !Text
    , packageResVersions :: !(NonEmpty Version)
    , packageResDependencies :: !(HashMap PkgId (DependencyRes a))
    }
    deriving (Show, Generic)


instance ToJSON (PackageRes 'V0) where
    toJSON PackageRes{..} =
        object
            [ "icon" .= encodeBase64 (snd packageResIcon)
            , "license" .= packageResLicense
            , "instructions" .= packageResInstructions
            , "manifest" .= packageResManifest
            , "categories" .= packageResCategories
            , "versions" .= packageResVersions
            , "dependency-metadata" .= packageResDependencies
            ]
instance ToJSON (PackageRes 'V1) where
    toJSON PackageRes{..} =
        object
            [ "icon" .= dataUrl packageResIcon
            , "license" .= packageResLicense
            , "instructions" .= packageResInstructions
            , "manifest" .= packageResManifest
            , "categories" .= packageResCategories
            , "versions" .= packageResVersions
            , "dependency-metadata" .= packageResDependencies
            ]


instance ToJSON (Sigma ApiVersion (TyCon PackageListRes)) where
    toJSON (s :&: t) = case s of
        SV0 -> toJSON t
        SV1 -> toJSON t
instance ToContent (Sigma ApiVersion (TyCon PackageListRes)) where
    toContent = toContent . toJSON
instance ToTypedContent (Sigma ApiVersion (TyCon PackageListRes)) where
    toTypedContent = toTypedContent . toJSON


type DependencyRes :: ApiVersion -> Type
data DependencyRes a = DependencyRes
    { dependencyResTitle :: !Text
    , dependencyResIcon :: !(ContentType, ByteString)
    }
    deriving (Eq, Show)


instance ToJSON (DependencyRes 'V0) where
    toJSON DependencyRes{..} = object ["icon" .= encodeBase64 (snd dependencyResIcon), "title" .= dependencyResTitle]
instance ToJSON (DependencyRes 'V1) where
    toJSON DependencyRes{..} = object ["icon" .= dataUrl dependencyResIcon, "title" .= dependencyResTitle]
