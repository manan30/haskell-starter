{-# OPTIONS -Wall -Wno-type-defaults #-}

module Homework1 where

{- ************************************************************ -}
{- ************************************************************ -}

{- List Functions -}

-- Haskell lists are "linked lists"
--   either  "nil/empty"  ( [] )
--   or "cons/non-empty"  ( x:xs )

listIsEmpty :: [a] -> Bool
listIsEmpty []      = True
listIsEmpty (_ : _) = False




