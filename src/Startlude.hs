module Startlude
  ( module X
  , module Startlude
  )
where

import           Control.Arrow      as X ((&&&))
import           Control.Comonad    as X
import           Control.Error.Util as X
import           Data.Coerce        as X
import           Data.String        as X (String, fromString)
import           Data.Time.Clock    as X
import           Protolude          as X hiding (bool, hush, isLeft, isRight, note, tryIO, (<.>))

id :: a -> a
id = identity
