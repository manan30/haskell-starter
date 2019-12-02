{-# OPTIONS -Wall -Wno-type-defaults #-}

module Problems1 where

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

listLength :: [a] -> Integer
listLength []       = 0
listLength (_ : xs) = 1 + listLength xs
-- equivalent to length

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

listAppend :: [a] -> [a] -> [a]
listAppend []       [] = []
listAppend xs       [] = xs
listAppend []       ys = ys
listAppend (x : xs) ys = x : listAppend xs ys

listReverse :: [a] -> [a]
listReverse xs = listReverse' xs []
 where
  listReverse' :: [a] -> [a] -> [a]
  listReverse' []       ys = ys
  listReverse' (y : ys) zs = listReverse' ys (y : zs)

{- ************************************************************ -}

listSwizzle :: [a] -> [a] -> [a]
listSwizzle []       ys       = ys
listSwizzle xs       []       = xs
listSwizzle (x : xs) (y : ys) = x : listAppend [y] (listSwizzle xs ys)

listSwizzleTests :: Test
listSwizzleTests = TestList
  [ listSwizzle [1, 2, 3, 4] [5, 6] ~?= [1, 5, 2, 6, 3, 4]
  , listSwizzle [5, 6] [1, 2, 3, 4] ~?= [5, 1, 6, 2, 3, 4]
  , listSwizzle [1, 2, 3] [4, 5, 6] ~?= [1, 4, 2, 5, 3, 6]
  , listSwizzle [1, 2, 3, 4] [5, 6] ~?= [1, 5, 2, 6, 3, 4]
  ]

listHasElem :: Eq a => [a] -> a -> Bool
listHasElem []       _ = False
listHasElem (x : xs) a = x == a || listHasElem xs a

listHasElemTests :: Test
listHasElemTests = TestList
  [ listHasElem [1, 2, 3] 2 ~?= True
  , listHasElem [1, 2, 3] 4 ~?= False
  , listHasElem ['A', 'B', 'C'] 'B' ~?= True
  , listHasElem ['A', 'B', 'C'] 'D' ~?= False
  ]

listHasDuplicates :: Eq a => [a] -> Bool
listHasDuplicates []       = False
-- listHasDuplicates (x : xs) = listHasElem (listTake (listLength xs) xs) x
listHasDuplicates (x : xs) = listHasElem xs x || listHasDuplicates xs

listHasDuplicatesTests :: Test
listHasDuplicatesTests = TestList
  [ listHasDuplicates [1, 2, 3] ~?= False
  , listHasDuplicates [1, 2, 1] ~?= True
  , listHasDuplicates ['A', 'B', 'C'] ~?= False
  , listHasDuplicates ['A', 'B', 'A'] ~?= True
  ]

{- ************************************************************ -}
{- ************************************************************ -}

problems1Tests :: Test
problems1Tests = TestList [listSwizzleTests, listHasDuplicatesTests]


