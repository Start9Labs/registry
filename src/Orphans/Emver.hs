{-# OPTIONS_GHC -fno-warn-orphans #-}
-- | This module is here for the express purpose of keeping ecosystem dependencies separate from the core library.
-- The core library should in theory be only dependent on base, text, and attoparsec. These are reasonable dependencies.
-- aeson, persistent, and yesod are not. So we put those here as they will not be extracted into a separate library.
module Orphans.Emver where

import           Startlude                      ( ($)
                                                , (.)
                                                , (<=<)
                                                , Applicative(pure)
                                                , Bifunctor(first)
                                                , either
                                                , show
                                                )

import           Data.Aeson                     ( FromJSON(parseJSON)
                                                , ToJSON(toJSON)
                                                , Value(String)
                                                , withText
                                                )
import qualified Data.Attoparsec.Text          as Atto

import           Control.Monad.Fail             ( MonadFail(fail) )
import qualified Data.Text                     as T
import           Database.Persist.Sql           ( PersistField(..)
                                                , PersistFieldSql(..)
                                                , PersistValue(PersistText)
                                                , SqlType(SqlString)
                                                )
import           Lib.Types.Emver                ( Version
                                                , VersionRange
                                                , parseRange
                                                , parseVersion
                                                )

instance FromJSON Version where
    parseJSON = withText "Emver Version" $ either fail pure . Atto.parseOnly parseVersion
instance FromJSON VersionRange where
    parseJSON = withText "Emver" $ either fail pure . Atto.parseOnly parseRange
instance ToJSON VersionRange where
    toJSON = String . show

instance PersistField Version where
    toPersistValue   = PersistText . show
    fromPersistValue = first T.pack . Atto.parseOnly parseVersion <=< fromPersistValue
instance PersistFieldSql Version where
    sqlType _ = SqlString
instance PersistField VersionRange where
    toPersistValue   = PersistText . show
    fromPersistValue = first T.pack . Atto.parseOnly parseRange <=< fromPersistValue
instance PersistFieldSql VersionRange where
    sqlType _ = SqlString
