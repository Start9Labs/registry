module Lib.Semver where

import           Startlude

import           Lib.Types.Semver

(<||) :: HasAppVersion a => a -> AppVersionSpecification -> Bool
(<||) a (AppVersionSpecification SVEquals av1)        = version a == av1
(<||) a (AppVersionSpecification SVLessThan av1)      = version a < av1
(<||) a (AppVersionSpecification SVGreaterThan av1)   = version a > av1
(<||) a (AppVersionSpecification SVLessThanEq av1)    = version a <= av1
(<||) a (AppVersionSpecification SVGreaterThanEq av1) = version a >= av1
(<||) a (AppVersionSpecification SVGreatestWithMajor av1)
    = major av == major av1 && av >= av1
    where
        av = version a
(<||) a (AppVersionSpecification SVGreatestWithMajorMinor av1)
    = major av == major av1 && minor av == minor av1 && av >= av1
    where
        av = version a

major :: AppVersion -> Word16
major (AppVersion (a, _, _)) = a
minor :: AppVersion -> Word16
minor (AppVersion (_, a, _)) = a
patch :: AppVersion -> Word16
patch (AppVersion (_, _, a)) = a

hasGiven :: (AppVersion -> Word16) -> AppVersion -> AppVersion -> Bool
hasGiven projection av = (== projection av) . projection

getSpecifiedAppVersion :: HasAppVersion a => AppVersionSpecification -> [a] -> Maybe a
getSpecifiedAppVersion avSpec = appVersionMax . filter (<|| avSpec)

class HasAppVersion a where
    version :: a -> AppVersion

instance HasAppVersion AppVersion where
    version = id

appVersionMax :: HasAppVersion a => [a] -> Maybe a
appVersionMax [] = Nothing
appVersionMax as = Just $ maximumBy (\a1 a2 -> version a1 `compare` version a2) as


