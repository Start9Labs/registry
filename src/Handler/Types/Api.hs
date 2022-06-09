module Handler.Types.Api where

import           GHC.Read                       ( Read(..) )
import           GHC.Show                       ( show )
import           Startlude                      ( Eq
                                                , Maybe(..)
                                                , Ord
                                                , Read
                                                , Show
                                                )
import           Yesod                          ( PathPiece(..) )

data ApiVersion
    = V0
    | V1 deriving (Eq, Ord)

instance Show ApiVersion where
    show V0 = "v0"
    show V1 = "v1"
instance Read ApiVersion where
    readsPrec = _


instance PathPiece ApiVersion where
    toPathPiece V0 = "v0"
    toPathPiece V1 = "v1"
    fromPathPiece "v0" = Just V0
    fromPathPiece "v1" = Just V1
    fromPathPiece _    = Nothing
