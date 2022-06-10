{-# LANGUAGE DataKinds #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Handler.Types.Api where

import Data.Ord.Singletons
import Data.Singletons.TH
import GHC.Read (Read (..))
import GHC.Show (show)
import Prelude.Singletons
import Startlude (
    Eq,
    Maybe (..),
    Ord,
    Show,
 )
import Yesod (PathPiece (..))


$( singletons
    [d|
        data ApiVersion
            = V0
            | V1
            deriving (Eq, Ord)
        |]
 )


instance Show ApiVersion where
    show V0 = "v0"
    show V1 = "v1"


instance Read ApiVersion where
    readsPrec _ "v0" = [(V0, "")]
    readsPrec _ "v1" = [(V1, "")]
    readsPrec _ _ = []


instance PathPiece ApiVersion where
    toPathPiece V0 = "v0"
    toPathPiece V1 = "v1"
    fromPathPiece "v0" = Just V0
    fromPathPiece "v1" = Just V1
    fromPathPiece _ = Nothing
