module Main where

import           Test.HUnit
import           Problems1


main :: IO ()
main = do
  _ <- runTestTT problems1Tests
  return ()
