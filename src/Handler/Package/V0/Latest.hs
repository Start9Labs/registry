module Handler.Package.V0.Latest where

import Data.Aeson (ToJSON (..))
import Data.HashMap.Strict (HashMap)
import Lib.Types.AppIndex (PkgId)
import Lib.Types.Emver (Version)
import Startlude (Generic, Maybe, Show, (.))
import Yesod (ToContent (..), ToTypedContent (..))


newtype VersionLatestRes = VersionLatestRes (HashMap PkgId (Maybe Version))
    deriving (Show, Generic)
instance ToJSON VersionLatestRes
instance ToContent VersionLatestRes where
    toContent = toContent . toJSON
instance ToTypedContent VersionLatestRes where
    toTypedContent = toTypedContent . toJSON
