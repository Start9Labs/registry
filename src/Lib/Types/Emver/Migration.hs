module Lib.Types.Emver.Migration (from, to, dead, navigate) where

import Control.Arrow (Arrow ((&&&), (***)))
import Data.Bool (Bool (..), not, otherwise, (&&))
import Data.Monoid ((<>))
import Lib.Types.Emver (
    Version,
    VersionRange (None),
    conj,
    exactly,
    (<||),
 )
import Protolude (($))
import Startlude (
    Alternative ((<|>)),
    Eq (..),
    Maybe (..),
    Num (..),
    Show,
    Traversable (traverse),
    Word,
    filter,
    flip,
    headMay,
    mapMaybe,
    pure,
    uncurry,
    (.),
 )


type MigrationGoal = (Version, Version)
data MigrationTactic = MigrationTactic
    { migrationTacticSourceRange :: VersionRange
    , migrationTacticTargetRange :: VersionRange
    , migrationTacticMidpoints :: [VersionRange]
    }
    deriving (Show)


from :: Version -> VersionRange -> MigrationTactic
from v range = MigrationTactic range (exactly v) []


to :: Version -> VersionRange -> MigrationTactic
to v range = MigrationTactic (exactly v) range []


(>>>) :: MigrationTactic -> MigrationTactic -> MigrationTactic
(MigrationTactic s t ms) >>> (MigrationTactic s' t' ms') = case conj t s' of
    None -> MigrationTactic None None []
    other -> MigrationTactic s t' (ms <> (other : ms'))


dead :: MigrationTactic -> Bool
dead (MigrationTactic None _ _) = True
dead (MigrationTactic _ None _) = True
dead _ = False


navigate :: [MigrationTactic] -> MigrationGoal -> [Version] -> Maybe [Version]
navigate = navigate' 1


navigate' :: Word -> [MigrationTactic] -> MigrationGoal -> [Version] -> Maybe [Version]
navigate' n tactics (source, target) avail =
    case headMay $ mapMaybe (traverse (flip select avail) . migrationTacticMidpoints) (filter (satisfactory . bounds) tactics) of
        Nothing -> if n == 0 then Nothing else navigate' (n - 1) composites (source, target) avail
        Just x -> Just x
    where
        bounds = migrationTacticSourceRange &&& migrationTacticTargetRange
        satisfactory = uncurry (&&) . ((source <||) *** (target <||))
        composites = do
            x <- tactics
            y <- tactics
            pure x <|> pure y <|> case (x >>> y, y >>> x) of
                (m@(MigrationTactic s t ms), m'@(MigrationTactic s' t' ms'))
                    | not (dead m) && not (dead m') -> [m, m']
                    | not (dead m) -> pure m
                    | not (dead m') -> pure m'
                    | otherwise -> []


select :: VersionRange -> [Version] -> Maybe Version
select range avail = headMay $ filter (<|| range) avail
