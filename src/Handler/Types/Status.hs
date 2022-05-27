{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}
module Handler.Types.Status where

import           Startlude                      ( (.)
                                                , Eq
                                                , Maybe
                                                , Show
                                                )

import           Data.Aeson                     ( KeyValue((.=))
                                                , ToJSON(toJSON)
                                                , object
                                                )
import           Yesod.Core.Content             ( ToContent(..)
                                                , ToTypedContent(..)
                                                )

import           Lib.Types.Emver                ( Version )
import           Orphans.Emver                  ( )

data AppVersionRes = AppVersionRes
    { appVersionVersion :: Version
    }
    deriving (Eq, Show)
instance ToJSON AppVersionRes where
    toJSON AppVersionRes { appVersionVersion } = object ["version" .= appVersionVersion]
instance ToContent AppVersionRes where
    toContent = toContent . toJSON
instance ToTypedContent AppVersionRes where
    toTypedContent = toTypedContent . toJSON
instance ToContent (Maybe AppVersionRes) where
    toContent = toContent . toJSON
instance ToTypedContent (Maybe AppVersionRes) where
    toTypedContent = toTypedContent . toJSON
