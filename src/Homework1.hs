{-# OPTIONS -Wall -Wno-type-defaults #-}

module Homework1 where

import           Test.HUnit

{- ************************************************************ -}
{- ************************************************************ -}

{- List Functions -}

-- Haskell lists are "linked lists"
--   either  "nil/empty"  ( [] )
--   or "cons/non-empty"  ( x:xs )

listIsEmpty :: [a] -> Bool
listIsEmpty []      = True
listIsEmpty (_ : _) = False
-- equivalent to null

listIsEmptyTests :: Test
listIsEmptyTests =
  TestList [listIsEmpty [] ~?= True, listIsEmpty [1, 2, 3] ~?= False]

listLength :: [a] -> Integer
listLength []       = 0
listLength (_ : xs) = 1 + listLength xs
-- equivalent to length

listLengthTests :: Test
listLengthTests = TestList [listLength [] ~?= 0, listLength [1, 2, 3] ~?= 3]

listSum :: Num a => [a] -> a
listSum []       = 0
listSum (x : xs) = x + listSum xs

listProduct :: Num a => [a] -> a
listProduct []       = 1
listProduct (x : xs) = x * listProduct xs

listTake :: Integer -> [a] -> [a]
listTake 0 _        = []
listTake _ []       = []
listTake n (x : xs) = x : listTake (n - 1) xs

listDrop :: Integer -> [a] -> [a]
listDrop 0 xs       = xs
listDrop _ []       = []
listDrop n (_ : xs) = listDrop (n - 1) xs

listZip :: ([a], [b]) -> [(a, b)]
listZip ([]    , []    ) = []
listZip (_     , []    ) = []
listZip ([]    , _     ) = []
listZip (x : xs, y : ys) = (x, y) : listZip (xs, ys)

listUnzip :: [(a, b)] -> ([a], [b])
listUnzip []             = ([], [])
listUnzip ((x, y) : xys) = (x : xs, y : ys) where (xs, ys) = listUnzip xys

{- ************************************************************ -}
{- ************************************************************ -}

homework1Tests :: Test
homework1Tests = TestList [listIsEmptyTests, listLengthTests]


