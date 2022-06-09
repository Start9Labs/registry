module Handler.Package.V0.Info where

import Data.Aeson (ToJSON (..))
import Startlude (Generic, Show, Text, (.))
import Yesod (ToContent (..), ToTypedContent (..))


data InfoRes = InfoRes
    { name :: !Text
    , categories :: ![Text]
    }
    deriving (Show, Generic)
instance ToJSON InfoRes
instance ToContent InfoRes where
    toContent = toContent . toJSON
instance ToTypedContent InfoRes where
    toTypedContent = toTypedContent . toJSON
