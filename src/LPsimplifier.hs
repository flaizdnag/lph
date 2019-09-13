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
simplifySameHeads cls = do
    let atoms    = sort $ nub $ concatMap (\x -> clPBody x ++ clNBody x) cls
        headAtom = clHead $ head cls
    valuation <- applyQM $ clsToValStrings cls
    return $ makeCl atoms headAtom (show valuation)


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
    | all ('-' ==) string = Fact head
    | otherwise           = Cl head pBody nBody
    where
        lenDiff = length atoms - length string
        balancedStr
            | lenDiff == 0 = string
            | otherwise    = replicate lenDiff '0' ++ string
        pBody = map fst $ filter (('1'==) . snd) $ zip atoms balancedStr
        nBody = map fst $ filter (('0'==) . snd) $ zip atoms balancedStr


-- | Application of the @qm@ algorithm.
applyQM xs = qm (map getTerm $ map fromString xs) [] []


--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------

{-
-- | Simplification of a logic program.
lpSimp :: LP -> LP
lpSimp lp = do
    sameHeads <- groupByHeads lp
    clause <- simplifySameHeads sameHeads
    return clause
{-
    where
        groupByHeads = groupBy (\x y -> clHead x == clHead y) . sortBy (\x y -> compare (clHead x) (clHead y))
-}
groupByHeads = groupBy (\x y -> clHead x == clHead y) . sortBy (\x y -> compare (clHead x) (clHead y))


simplifySameHeads :: [Clause] -> [Clause]
simplifySameHeads cls
    | existUnrelated = simplifySameHeads ready
    | otherwise      = ready
    where
        (ClsToSimplify _ _ _ ready) =
            clausesSimplifier (ClsToSimplify (uniquePairs cls) [] [] [])
        existUnrelated = any ((Unrelated /=) . checkClsRelation) (uniquePairs ready)


checkClsRelation :: (Clause, Clause) -> BodiesRelation
checkClsRelation (cl1, cl2) = case (cl1 == cl2 || clHead cl1 /= clHead cl2) of
    True                  -> Unrelated
    False
        | subBody cl1 cl2 -> SubBodiesFinS
        | subBody cl2 cl1 -> SubBodiesSinF
        | comBody cl1 cl2 -> Complementary
        | otherwise       -> Unrelated
    where
        subBody x y =
            (isSublist (clPBody x) (clPBody y) && (clNBody x == clNBody y)) ||
            (isSublist (clNBody x) (clNBody y) && (clPBody x == clPBody y))
        comBody x y =
            isSublist (symDifference (clPBody x) (clPBody y)) (symDifference (clNBody x) (clNBody y))


clausesSimplifier :: ClausesToSimplify -> ClausesToSimplify
clausesSimplifier (ClsToSimplify [] done alone ready) = ClsToSimplify [] done [] (ready++(nub $ filter (\x -> not $ elem x done) alone))
clausesSimplifier (ClsToSimplify ((cl1, cl2):pairs) done alone ready)
    | isDone cl1 && isDone cl2         = nextIter [] [] []
    | isDone cl1 && (not $ isDone cl2) = nextIter [] [cl2] []
    | isDone cl2 && (not $ isDone cl1) = nextIter [] [cl1] []
    | otherwise =
        case (checkClsRelation (cl1, cl2)) of
            Unrelated     -> nextIter [cl1, cl2] [] [cl1, cl2]
            SubBodiesFinS -> nextIter [cl1, cl2] [] [cl1]
            SubBodiesSinF -> nextIter [cl1, cl2] [] [cl2]
            Complementary -> nextIter [cl1, cl2] [] [merged]
    where
        nextIter xs ys zs =
            clausesSimplifier (ClsToSimplify pairs (xs++done) (ys++alone) (zs++ready))
        isDone x       = elem x done
        merged
            | emptyIntersection = Fact (clHead cl1)
            | otherwise         = Cl (clHead cl1) intersectPbody intersectNbody
        intersectPbody = intersect (clPBody cl1) (clPBody cl2)
        intersectNbody = intersect (clNBody cl1) (clNBody cl2)
        emptyIntersection = null intersectPbody && null intersectNbody 
-}
