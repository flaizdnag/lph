{-|
Module      : LPsimplifier
Description : Logic programs simplifier based on the Quine-McCluskey algorithm.
Copyright   : (c) Andrzej G., 2017-
License     : GPL-3
Maintainer  : andrzej.m.gajda@gmail.com
Stability   : experimental
Portability : POSIX

The implementation of the Quine-McCluskey algorithm is taken form:
https://hackage.haskell.org/package/qm-0.1.0.0/candidate
-}
module LPsimplifier (simplifyLP) where

import Qm
import LogicPrograms
import Data.List (groupBy, sortBy, nub, sort)


-- | Simplification of a logic program by means of the Quine-McCluskey
-- algorithm.
-- NOTE: bodies of clauses have to have the same length.
simplifyLP :: LP -> LP
simplifyLP lp = do
    sameHeads <- grouped $ sorted lp
    clause <- simplifySameHeads sameHeads
    return clause
    where
        sorted  = sortBy (\x y -> compare (clHead x) (clHead y))
        grouped = groupBy (\x y -> clHead x == clHead y)


-- | Reduction of a list of clauses that have the same head.
simplifySameHeads :: [Clause] -> [Clause]
simplifySameHeads cls
    | anyFact   = [Fact (clHead $ head cls)]
    | otherwise = do

        let atoms    = sort $ lpBodies cls
            headAtom = clHead $ head cls
        
        valuation <- applyQM $ clsToValStrings cls
        return $ makeCl atoms headAtom (show valuation)

    where
        anyFact = any (\x -> Fact (clHead x) == x) cls


-- | Translating a list of clauses into a list of strings that represent bodies
-- of the clauses.
clsToValStrings :: [Clause] -> [String]
clsToValStrings cls = do
    (pBody, nBody) <- map (\c -> (clPBody c, clNBody c)) cls
    
    let asVals    = zip pBody (repeat 1) ++ zip nBody (repeat 0)
        ordAsVals = sortBy (\x y -> compare (fst x) (fst y)) asVals
        vals      = filter (' ' /=) $ unwords $ map (show . snd) ordAsVals

    return vals


-- | Creating a clause from the output from the @qm@ algorithm.
makeCl :: [Atom] -> Atom -> String -> Clause
makeCl atoms head string
    | all ('-' ==) balancedStr = Fact head
    | otherwise                = Cl head pBody nBody
    where
        lenDiff = length atoms - length string
        balancedStr
            | lenDiff == 0 = string
            | otherwise    = replicate lenDiff '0' ++ string
        pBody = map fst $ filter (('1'==) . snd) $ zip atoms balancedStr
        nBody = map fst $ filter (('0'==) . snd) $ zip atoms balancedStr


-- | Application of the @qm@ algorithm.
applyQM xs = qm (map getTerm $ map fromString xs) [] []
