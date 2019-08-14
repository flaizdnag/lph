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

Module that contains auxiliary functions used in other modules.
-}
module Auxiliary
    ( isSublist
    , isProperSublist
    , jointElem
    , eqLists
    , ltLists
    , first
    , second
    , third
    , symDifference
    , uniquePairs
    ) where

import Data.List ((\\), intersect, sort)


-- | Checks if a list is a sublist of the other list (ordering does not matter).
isSublist :: Eq a => [a] -> [a] -> Bool
isSublist xs ys = null (xs \\ ys)


-- | Checks if a given list is contained in the second list, while the second
-- list is bigger than the first.
isProperSublist :: Eq a => [a] -> [a] -> Bool
isProperSublist xs ys = isSublist xs ys && (not $ null (ys \\ xs))


-- | Checks if a list contains any element of the other list (ordering does not
-- matter).
jointElem :: Eq a => [a] -> [a] -> Bool
jointElem xs ys = not $ null (intersect xs ys)


-- | Checks if a list has the same elements as the other list (ordering does not
-- matter).
eqLists :: Ord a => [a] -> [a] -> Bool
eqLists xs ys = sort xs == sort ys


-- | Checks if a sorted list is ordered lower than the other list.
ltLists :: Ord a => [a] -> [a] -> Bool
ltLists xs ys = sort xs < sort ys


-- | Returns the first element from a triple.
first :: (a, b, c) -> a
first (x, _, _) = x


-- | Returns the second element from a triple.
second :: (a, b, c) -> b
second (_, x, _) = x


-- | Returns the third element from a triple.
third :: (a, b, c) -> c
third (_, _, x) = x


-- | Symmetric difference between two lists.
symDifference :: Eq a => [a] -> [a] -> [a]
symDifference xs ys = (xs \\ ys) ++ (ys \\ xs)


-- | Unique pairs of all elements in the given list.
uniquePairs :: Ord a => [a] -> [(a, a)]
uniquePairs list = [ (el1, el2) | el1 <- list, el2 <- list, el1 < el2]
