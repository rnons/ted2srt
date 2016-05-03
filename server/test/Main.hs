module Main where

import LoadEnv (loadEnvFrom)
import Test.Hspec.Runner
import Test.Hspec.Formatters

import qualified Spec


main :: IO ()
main = do
    loadEnvFrom ".env.test"
    hspecWith defaultConfig {configFormatter = Just progress} Spec.spec
    return ()
