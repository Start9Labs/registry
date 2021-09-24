module Main where

import           Test.Hspec.Runner
import qualified Spec
import           Test.Hspec.Formatters
import           Startlude
import           GHC.IO.Encoding


main :: IO ()
main = do
    setLocaleEncoding utf8
    hspecWith defaultConfig { configFormatter = Just progress } Spec.spec
