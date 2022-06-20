module Startlude (
    module X,
    module Startlude,
) where

import Control.Arrow as X (
    (&&&),
 )
import Control.Error.Util as X
import Data.Coerce as X
import Data.String as X (
    String,
    fromString,
 )
import Data.Time.Clock as X
import Protolude as X hiding (
    bool,
    hush,
    isLeft,
    isRight,
    note,
    readMaybe,
    tryIO,
    (<.>),
 )
import Protolude qualified as P (
    readMaybe,
 )


id :: a -> a
id = identity


readMaybe :: (Read a) => Text -> Maybe a
readMaybe = P.readMaybe
{-# INLINE readMaybe #-}


maximumOn :: forall a b t. (Ord b, Foldable t) => (a -> b) -> t a -> Maybe a
maximumOn f = foldr (\x y -> maxOn f x <$> y <|> Just x) Nothing


maxOn :: Ord b => (a -> b) -> a -> a -> a
maxOn f x y = if f x > f y then x else y


{-# INLINE (.*) #-}
infixr 8 .*
(.*) :: (b -> c) -> (a1 -> a2 -> b) -> a1 -> a2 -> c
(.*) = (.) . (.)
