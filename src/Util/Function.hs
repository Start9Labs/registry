module Util.Function where

import           Startlude

(.*) :: (b -> c) -> (a0 -> a1 -> b) -> a0 -> a1 -> c
(.*) = (.) . (.)

(.**) :: (b -> c) -> (a0 -> a1 -> a2 -> b) -> a0 -> a1 -> a2 -> c
(.**) = (.) . (.*)