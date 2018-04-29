module Main where

import LoadEnv (loadEnvFrom)
import System.Exit (ExitCode(..))
import System.Process (system)
import Test.Hspec.Runner
import Test.Hspec.Formatters

import qualified Spec


main :: IO ()
main = do
    loadEnvFrom ".env.test"
    ExitSuccess <- system "test/before.sh"
    hspecWith defaultConfig {configFormatter = Just progress} Spec.spec
    return ()
