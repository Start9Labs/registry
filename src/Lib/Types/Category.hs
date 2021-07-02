{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}


module Lib.Types.Category where

import Startlude 
import Database.Persist.Postgresql
import Data.Aeson
import Control.Monad
import Yesod.Core
import Data.String.Interpolate.IsString
import qualified Data.ByteString.Lazy     as BS

data CategoryTitle = FEATURED 
        | BITCOIN
        | LIGHTNING
        | DATA
        | MESSAGING
        | NONE
        | ANY
    deriving (Eq, Show, Enum, Read)
instance PersistField CategoryTitle where
    fromPersistValue = fromPersistValueJSON
    toPersistValue = toPersistValueJSON
instance PersistFieldSql CategoryTitle where
  sqlType _ = SqlString
instance ToJSON CategoryTitle where
    toJSON = String . show
instance FromJSON CategoryTitle where
    parseJSON = withText "CategoryTitle" $ \case
        "FEATURED"   -> pure FEATURED
        "BITCOIN"    -> pure BITCOIN
        "LIGHTNING"  -> pure LIGHTNING
        "DATA"       -> pure DATA
        "MESSAGING"  -> pure MESSAGING
        "NONE"       -> pure NONE
        "ANY"        -> pure ANY
        _            -> fail "unknown category title"
instance ToContent CategoryTitle where
    toContent = toContent . toJSON
instance ToTypedContent CategoryTitle where
    toTypedContent = toTypedContent . toJSON

cat :: BS.ByteString
cat = [i|"featured"|]

