{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

module Lib.Registry where

import           Startlude

import qualified GHC.Read                       ( Read(..) )
import qualified GHC.Show                       ( Show(..) )
import           System.FilePath
import           Yesod.Core

newtype Extension (a :: Symbol) = Extension String deriving (Eq)
type S9PK = Extension "s9pk"

extension :: KnownSymbol a => Extension a -> String
extension = symbolVal

instance KnownSymbol a => Show (Extension a) where
    show e@(Extension file) = file <.> extension e

instance KnownSymbol a => Read (Extension a) where
    readsPrec _ s = case symbolVal $ Proxy @a of
        ""    -> [(Extension s, "")]
        other -> [ (Extension file, "") | ext' == "" <.> other ]
        where (file, ext') = splitExtension s

instance KnownSymbol a => PathPiece (Extension a) where
    fromPathPiece = readMaybe . toS
    toPathPiece   = show
