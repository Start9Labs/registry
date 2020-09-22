module Lib.Semver where

import           Startlude

import           Lib.Types.Semver

(<||) :: HasAppVersion a => a -> AppVersionSpec -> Bool
(<||) _ AppVersionAny                            = True
(<||) a (AppVersionSpec SVEquals            av1) = version a == av1
(<||) a (AppVersionSpec SVLessThan          av1) = version a < av1
(<||) a (AppVersionSpec SVGreaterThan       av1) = version a > av1
(<||) a (AppVersionSpec SVLessThanEq        av1) = version a <= av1
(<||) a (AppVersionSpec SVGreaterThanEq     av1) = version a >= av1
(<||) a (AppVersionSpec SVGreatestWithMajor av1) = major av == major av1 && av >= av1 -- "maj.*"
    where av = version a
(<||) a (AppVersionSpec SVGreatestWithMajorMinor av1) = major av == major av1 && minor av == minor av1 && av >= av1 -- "maj.min.*"
    where av = version a

major :: AppVersion -> Word16
major (AppVersion (a, _, _, _)) = a
minor :: AppVersion -> Word16
minor (AppVersion (_, a, _, _)) = a
patch :: AppVersion -> Word16
patch (AppVersion (_, _, a, _)) = a
build :: AppVersion -> Word16
build (AppVersion (_, _, _, a)) = a

hasGiven :: (AppVersion -> Word16) -> AppVersion -> AppVersion -> Bool
hasGiven projection av = (== projection av) . projection

getSpecifiedAppVersion :: HasAppVersion a => AppVersionSpec -> [a] -> Maybe a
getSpecifiedAppVersion avSpec = appVersionMax . filter (<|| avSpec)

class HasAppVersion a where
    version :: a -> AppVersion

instance HasAppVersion AppVersion where
    version = id

appVersionMax :: HasAppVersion a => [a] -> Maybe a
appVersionMax [] = Nothing
appVersionMax as = Just $ maximumBy (compare `on` version) as
