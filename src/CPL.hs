{-# LANGUAGE MultiParamTypeClasses #-}

{-|
Module      : CPL
Description : Classical Propositional Logic
Copyright   : (c) Aleksandra Cz., 2017
                  Kinga O., 2017
                  Andrzej G., 2017
License     : GPL-3
Maintainer  : andrzej.m.gajda@gmail.com
Stability   : experimental
Portability : POSIX

Longer description
-}
module CPL
    ( Form (..)
    , IntCPL (..)
    , isModel2vCPL
    , isModel3vCPL
    ) where

import LogicPrograms
import Auxiliary
import TwoValuedSem
import ThreeValuedSem
import Data.List (sort, groupBy, (\\), union, foldl1', sortBy, nub)


-- | The CPL language.
data Form = V Atom 
          | N Form        -- negation
          | C [Form]      -- conjunction 
          | D [Form]      -- disjunction 
          | E Form Form   -- equivalence
          | T             -- verum
    deriving (Show, Read)

instance Eq Form where
    T     == T      = True
    V a   == V b    = a == b
    N x   == N y    = x == y
    C xs  == C ys   = eqLists xs ys
    D xs  == D ys   = eqLists xs ys
    E a b == E c d  = (a == c && b == d) || (a == d && b == c)
    _     == _      = False

instance Ord Form where
    T     < _       = True
    V a   < V b     = a < b
    N x   < N y     = x < y
    C xs  < C ys    = sort xs < sort ys
    D xs  < D ys    = sort xs < sort ys
    E a b < E c d   = (a < c) -- should be enough...
    _     < _       = False

    a <= b = (a < b) || (a == b)
    a >  b = b < a
    a >= b = b <= a

instance TwoValuedSemantic Form IntCPL where
    eval2v f int = case f of
        T                   -> Tr2v
        V a
            | isVTr (V a)   -> Tr2v
            | otherwise     -> Fa2v
        N x
            | isTr x        -> Fa2v
            | otherwise     -> Tr2v
        C xs
            | any isFa xs   -> Fa2v
            | otherwise     -> Tr2v
        D xs
            | any isTr xs   -> Tr2v
            | otherwise     -> Fa2v
        E x y
            | sameEval x y  -> Tr2v
            | otherwise     -> Fa2v
        where
            isVTr    = \x -> elem (x :: Form) (trCPL int)
            isTr     = \x -> eval2v (x :: Form) int == Tr2v
            isFa     = \x -> eval2v (x :: Form) int == Fa2v
            sameEval = \x y -> eval2v (x :: Form) int == eval2v (y :: Form) int

instance ThreeValuedSemantic Form IntCPL where
    eval3v f int = case f of
        T                      -> Tr3v
        V a
            | isVTr (V a)      -> Tr3v
            | isVFa (V a)      -> Fa3v
            | otherwise        -> Un3v
        N x
            | isTr x           -> Fa3v
            | isFa x           -> Tr3v
            | otherwise        -> Un3v
        C xs
            | any isFa xs      -> Fa3v
            | any isUn xs      -> Un3v
            | otherwise        -> Tr3v
        D xs
            | any isTr xs      -> Tr3v
            | any isUn xs      -> Un3v
            | otherwise        -> Fa3v
        E x y
            | sameEval x y     -> Tr3v
            | isUn x || isUn y -> Un3v
            | otherwise        -> Fa3v
        where
            isVTr    = \x -> elem (x :: Form) (trCPL int)
            isVFa    = \x -> elem (x :: Form) (faCPL int)
            isTr     = \x -> eval3v (x :: Form) int == Tr3v
            isFa     = \x -> eval3v (x :: Form) int == Fa3v
            isUn     = \x -> eval3v (x :: Form) int == Un3v
            sameEval = \x y -> eval3v (x :: Form) int == eval3v (y :: Form) int


-- | An interpretation is a tuple with lists of variables: the first list
-- contains variables that are mapped to 'truth' and the second those that are
-- mapped to 'false'.
data IntCPL = Int { trCPL :: [Form] , faCPL :: [Form] }
    deriving (Show, Read, Eq)


-- | Checks if a given interpretation is a model for a given set of formulas.
isModel2vCPL :: [Form] -> IntCPL -> Bool
isModel2vCPL set int = all (\x -> eval2v x int == Tr2v) set

-- | Checks if a given interpretation is a model for a given set of formulas.
isModel3vCPL :: [Form] -> IntCPL -> Bool
isModel3vCPL set int = all (\x -> eval3v x int == Tr3v) set
