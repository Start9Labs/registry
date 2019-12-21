{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE RecordWildCards #-}
module Handler.Types.Register where

import           Startlude

import           Control.Monad.Fail
import           Data.Aeson
import           Data.ByteArray.Encoding
import           Data.ByteArray.Sized

data RegisterReq = RegisterReq
    { registerProductKey :: Text
    , registerPubKey     :: SizedByteArray 33 ByteString
    } deriving (Eq, Show)
instance FromJSON RegisterReq where
    parseJSON = withObject "Register Request" $ \o -> do
        registerProductKey <- o .: "productKey"
        registerPubKey <- o .: "pubKey" >>= \t ->
            case sizedByteArray <=< hush . convertFromBase Base16 $ encodeUtf8 t of
                Nothing -> fail "Invalid Hex Encoded Public Key"
                Just x  -> pure x
        pure RegisterReq{..}
