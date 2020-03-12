module Main where

import          Test.Hspec.Runner
import qualified Spec
import          Test.Hspec.Formatters
import qualified Handler.AppSpec as A

main :: IO ()
main = do
    _ <- A.spec
    hspecWith defaultConfig { configFormatter = Just progress } Spec.spec
