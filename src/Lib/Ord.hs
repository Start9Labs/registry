module Lib.Ord where

import Startlude (Alternative ((<|>)), Foldable (foldr), Maybe (..), Ord ((>)), (<$>))


maximumOn :: forall a b t. (Ord b, Foldable t) => (a -> b) -> t a -> Maybe a
maximumOn f = foldr (\x y -> maxOn f x <$> y <|> Just x) Nothing


maxOn :: Ord b => (a -> b) -> a -> a -> a
maxOn f x y = if f x > f y then x else y