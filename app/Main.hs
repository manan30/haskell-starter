module Main where

import           Test.HUnit
import           Homework1


main :: IO ()
main = do
  _ <- runTestTT homework1Tests
  return ()
