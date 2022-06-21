module Main where

import           GHC.IO.Encoding
import qualified Lib.Types.EmverProp           as EmverProp
import qualified Spec
import           Startlude
import           Test.Hspec.Formatters
import           Test.Hspec.Runner


main :: IO ()
main = do
    setLocaleEncoding utf8
    EmverProp.tests
    hspecWith defaultConfig { configFormatter = Just progress } Spec.spec
