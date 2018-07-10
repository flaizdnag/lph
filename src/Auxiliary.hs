{-|
Module      : Auxiliary
Description : Auxiliary functions.
Copyright   : (c) Aleksandra Cz., 2017--2018
                  Kinga O., 2017--2018
                  Andrzej G., 2017--2018
License     : GPL-3
Maintainer  : andrzej.m.gajda@gmail.com
Stability   : experimental
Portability : POSIX

Longer description
-}
module Auxiliary
    ( isSublist
    , isElem
    )where

-- | checks if list is a sublist of other list
-- intersect
isSublist :: Eq a => [a] -> [a] -> Bool
isSublist [] _ = True
isSublist (x:xs) ys
                    | elem x ys = isSublist xs ys
                    | otherwise = False


-- | checks if list contains any element of other list
isElem :: Eq a => [a] -> [a] -> Bool
isElem [] _ = False
isElem (x:xs) ys
                | elem x ys = True
                | otherwise = isElem xs ys
