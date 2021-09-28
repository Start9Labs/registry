module Util.Function where

import           Startlude

(.*) :: (b -> c) -> (a0 -> a1 -> b) -> a0 -> a1 -> c
(.*) = (.) . (.)

(.**) :: (b -> c) -> (a0 -> a1 -> a2 -> b) -> a0 -> a1 -> a2 -> c
(.**) = (.) . (.*)

preimage :: Eq b => (a -> b) -> b -> [a] -> [a]
preimage f target = filter ((== target) . f)

mapFind :: ([a] -> Maybe a) -> (b -> a) -> [b] -> Maybe b
mapFind _ _ [] = Nothing
mapFind finder mapping (b : bs) =
    let mB = mapFind finder mapping bs
        mA = finder [mapping b]
    in  case (mB, mA) of
            (Just b', _     ) -> Just b'
            (Nothing, Just _) -> Just b
            _                 -> Nothing

(<<&>>) :: (Functor f, Functor g) => f (g a) -> (a -> b) -> f (g b)
f <<&>> fab = fmap (fmap fab) f

