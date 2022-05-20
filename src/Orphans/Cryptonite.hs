{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE TypeApplications #-}
module Orphans.Cryptonite where
import           Crypto.Hash                    ( Digest
                                                , digestFromByteString
                                                )
import           Crypto.Hash.Algorithms         ( SHA256 )
import           Data.ByteArray.Encoding        ( Base(Base16)
                                                , convertFromBase
                                                , convertToBase
                                                )
import           Data.Text                      ( pack )
import           Database.Persist               ( PersistField(..)
                                                , PersistValue(PersistText)
                                                , SqlType(SqlString)
                                                )
import           Database.Persist.Sql           ( PersistFieldSql(..) )
import           Startlude                      ( ($)
                                                , (.)
                                                , Bifunctor(bimap, first)
                                                , ByteString
                                                , Either(Left)
                                                , Semigroup((<>))
                                                , decodeUtf8
                                                , encodeUtf8
                                                , join
                                                , note
                                                , show
                                                )

instance PersistField (Digest SHA256) where
    toPersistValue = PersistText . decodeUtf8 . convertToBase Base16
    fromPersistValue (PersistText t) =
        join
            . bimap pack (note "Invalid SHA256 Digest" . digestFromByteString)
            . convertFromBase @_ @ByteString Base16
            . encodeUtf8
            $ t
    fromPersistValue v = Left $ "Invalid PersistValue type: " <> show v

instance PersistFieldSql (Digest SHA256) where
    sqlType _ = SqlString
