{-# LANGUAGE RecordWildCards #-}

module Handler.Package.V0.ReleaseNotes where

import Data.Aeson (ToJSON (..), Value (..), object, (.=))
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HM
import Lib.Types.Emver (Version)
import Startlude (Eq, Show, Text, (.))
import Yesod (ToContent (..), ToTypedContent (..))


newtype ReleaseNotes = ReleaseNotes {unReleaseNotes :: HashMap Version Text}
    deriving (Eq, Show)
instance ToJSON ReleaseNotes where
    toJSON ReleaseNotes{..} = toJSON unReleaseNotes
instance ToContent ReleaseNotes where
    toContent = toContent . toJSON
instance ToTypedContent ReleaseNotes where
    toTypedContent = toTypedContent . toJSON
