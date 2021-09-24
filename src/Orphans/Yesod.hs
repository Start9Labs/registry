{-# OPTIONS_GHC -fno-warn-orphans #-}
module Orphans.Yesod where

import           Startlude

import           Yesod.Core

-- | Forgive me for I have sinned
instance ToJSON a => ToContent [a] where
    toContent = toContent . toJSON . fmap toJSON
instance ToJSON a => ToTypedContent [a] where
    toTypedContent = toTypedContent . toJSON . fmap toJSON

