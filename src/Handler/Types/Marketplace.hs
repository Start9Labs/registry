{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveGeneric #-}

module Handler.Types.Marketplace where
import           Data.Aeson
import qualified Data.HashMap.Internal.Strict  as HM
import           Lib.Types.AppIndex             ( PkgId )
import           Lib.Types.Category             ( CategoryTitle )
import           Lib.Types.Emver                ( Version
                                                , VersionRange
                                                )
import           Model                          ( Category
                                                , PkgDependency
                                                , PkgRecord
                                                , VersionRecord
                                                )
import           Startlude
import           Yesod


type URL = Text
newtype CategoryRes = CategoryRes {
    categories :: [CategoryTitle]
} deriving (Show, Generic)
instance ToJSON CategoryRes
instance ToContent CategoryRes where
    toContent = toContent . toJSON
instance ToTypedContent CategoryRes where
    toTypedContent = toTypedContent . toJSON
data PackageRes = PackageRes
    { packageResIcon         :: URL
    , packageResManifest     :: Data.Aeson.Value -- PackageManifest
    , packageResCategories   :: [CategoryTitle]
    , packageResInstructions :: URL
    , packageResLicense      :: URL
    , packageResVersions     :: [Version]
    , packageResDependencies :: HM.HashMap PkgId DependencyRes
    }
    deriving (Show, Generic)
newtype ReleaseNotes = ReleaseNotes { unReleaseNotes :: HM.HashMap Version Text }
    deriving (Eq, Show)
instance ToJSON ReleaseNotes where
    toJSON ReleaseNotes {..} = object [ t .= v | (k, v) <- HM.toList unReleaseNotes, let (String t) = toJSON k ]
instance ToContent ReleaseNotes where
    toContent = toContent . toJSON
instance ToTypedContent ReleaseNotes where
    toTypedContent = toTypedContent . toJSON
instance ToJSON PackageRes where
    toJSON PackageRes {..} = object
        [ "icon" .= packageResIcon
        , "license" .= packageResLicense
        , "instructions" .= packageResInstructions
        , "manifest" .= packageResManifest
        , "categories" .= packageResCategories
        , "versions" .= packageResVersions
        , "dependency-metadata" .= packageResDependencies
        ]
instance FromJSON PackageRes where
    parseJSON = withObject "PackageRes" $ \o -> do
        packageResIcon         <- o .: "icon"
        packageResLicense      <- o .: "license"
        packageResInstructions <- o .: "instructions"
        packageResManifest     <- o .: "manifest"
        packageResCategories   <- o .: "categories"
        packageResVersions     <- o .: "versions"
        packageResDependencies <- o .: "dependency-metadata"
        pure PackageRes { .. }
data DependencyRes = DependencyRes
    { dependencyResTitle :: Text -- TODO switch to `Text` to display actual title in Marketplace. Confirm with FE that this will not break loading. Perhaps return title and id?
    , dependencyResIcon  :: URL
    }
    deriving (Eq, Show)
instance ToJSON DependencyRes where
    toJSON DependencyRes {..} = object ["icon" .= dependencyResIcon, "title" .= dependencyResTitle]
instance FromJSON DependencyRes where
    parseJSON = withObject "DependencyRes" $ \o -> do
        dependencyResIcon  <- o .: "icon"
        dependencyResTitle <- o .: "title"
        pure DependencyRes { .. }
newtype PackageListRes = PackageListRes [PackageRes]
    deriving (Generic)
instance ToJSON PackageListRes
instance ToContent PackageListRes where
    toContent = toContent . toJSON
instance ToTypedContent PackageListRes where
    toTypedContent = toTypedContent . toJSON

newtype VersionLatestRes = VersionLatestRes (HM.HashMap PkgId (Maybe Version))
    deriving (Show, Generic)
instance ToJSON VersionLatestRes
instance ToContent VersionLatestRes where
    toContent = toContent . toJSON
instance ToTypedContent VersionLatestRes where
    toTypedContent = toTypedContent . toJSON
data OrderArrangement = ASC | DESC
    deriving (Eq, Show, Read)
data PackageListDefaults = PackageListDefaults
    { packageListOrder      :: OrderArrangement
    , packageListPageLimit  :: Int -- the number of items per page
    , packageListPageNumber :: Int -- the page you are on
    , packageListCategory   :: Maybe CategoryTitle
    , packageListQuery      :: Text
    }
    deriving (Eq, Show, Read)
data EosRes = EosRes
    { eosResVersion      :: Version
    , eosResHeadline     :: Text
    , eosResReleaseNotes :: ReleaseNotes
    }
    deriving (Eq, Show, Generic)
instance ToJSON EosRes where
    toJSON EosRes {..} =
        object ["version" .= eosResVersion, "headline" .= eosResHeadline, "release-notes" .= eosResReleaseNotes]
instance ToContent EosRes where
    toContent = toContent . toJSON
instance ToTypedContent EosRes where
    toTypedContent = toTypedContent . toJSON

data PackageReq = PackageReq
    { packageReqId      :: PkgId
    , packageReqVersion :: VersionRange
    }
    deriving Show
instance FromJSON PackageReq where
    parseJSON = withObject "package version" $ \o -> do
        packageReqId      <- o .: "id"
        packageReqVersion <- o .: "version"
        pure PackageReq { .. }
data PackageMetadata = PackageMetadata
    { packageMetadataPkgRecord         :: Entity PkgRecord
    , packageMetadataPkgVersionRecords :: [Entity VersionRecord]
    , packageMetadataPkgCategories     :: [Entity Category]
    , packageMetadataPkgVersion        :: Version
    }
    deriving (Eq, Show)
data PackageDependencyMetadata = PackageDependencyMetadata
    { packageDependencyMetadataPkgDependencyRecord :: Entity PkgDependency
    , packageDependencyMetadataDepPkgRecord        :: Entity PkgRecord
    , packageDependencyMetadataDepVersions         :: [Entity VersionRecord]
    }
    deriving (Eq, Show)
