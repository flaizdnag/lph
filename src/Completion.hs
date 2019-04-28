{-|
Module      : Completion
Description : Tools needed to perform Clark's completion for a logic program.
Copyright   : (c) Aleksandra Cz., 2017
                  Kinga O., 2017
                  Andrzej G., 2017
License     : GPL-3
Maintainer  : andrzej.m.gajda@gmail.com
Stability   : experimental
Portability : POSIX

Module that contains tools needed to create Clark's completion for a logic
program, which is a list of formulas of the classical propositional logic (CPL).
In addition, there are also tools for searching for a model for the completion
of a logic program, and tools to test if a given interpretation is a model for
the completion of a logic program.
-}
module Completion
    ( comp
    , intLPtoIntCPL
    , makeModels
    , modelCheckCPL
    ) where

import LogicPrograms
import Auxiliary
import ThreeValuedSem
import CPL
import Data.List (sort, groupBy, (\\), union, foldl1', sortBy, nub)


-- | Turns a list of atoms into a list of variables.
asToVar :: [Atom] -> [Form]
asToVar = map (\x -> V x)

-- | Turns a list of atoms into a list of negated variables.
asToNVar :: [Atom] -> [Form]
asToNVar = map (\x -> N (V x))

-- | Creating Clark's completion for a logic program.
comp :: LP -> [Form]
comp lp = eqs ++ negVars
    where
        -- list of definitions (clauses) for all heads from the logic program;
        -- as a result we obtain a list of lists of clauses
        hdsDfs   = map (\x -> atomDef x lp) (lpHeads lp)
        -- turning a clause into a tuple with head of the clause and the body of
        -- the clause as a conjunction
        con      = \cl -> case cl of
            Fact h          -> (V h, C [T])
            Assumption h    -> (V h, C [F])
            Cl h pb nb      -> (V h, C (asToVar pb ++ asToNVar nb))
        -- mapping 'con' function over a list of clauses
        mapCon   = \x -> map con x
        -- turning a list of tuples with head of a clause and the body as
        -- a conjunction into an equivalence, where one part is the head, while
        -- the other is a disjunction of conjunctions from all of the tuples;
        -- NOTICE: the assumption here is that the list contains tuples with the
        -- same first element, i.e. with the same head
        eq       = \xs -> E (fst (head xs)) (D (map snd xs))
        -- mapping functions 'mapCon' and 'eq' (in that order) over the set of
        -- definitions for heads from the logic program; as a result we obtain
        -- all equivalences for the Clark's completion
        eqs      = map (eq . mapCon) hdsDfs
        -- creating a list of negated variables for those atoms that do not
        -- occur as a head in the logic program
        negVars  = asToNVar ((bp lp) \\ (lpHeads lp))

-- | Creates an interpretation for CPL from an interpretation for LP.
intLPtoIntCPL :: IntLP -> IntCPL
intLPtoIntCPL int = IntCPL (asToVar (trLP int)) (asToVar (faLP int))

--------------------------------------------------------------------------------
-- Looking for a model for Clark's completion                                 --
--------------------------------------------------------------------------------

-- | Takes a formula and an interpretation and returns the value of the formula.
-- The interpretation is a tuple with lists of variables: 'true' in the first
-- list and 'false' in the second list.
-- We use three values: 'Tr', 'Fa' and 'Un', but this is not a standard
-- approach, because the 'Un' value serves only informational purposes,
-- therefore this function is different from the Łukasiewicz approach (the
-- difference is in the evaluation of the equivalence).
evalCPL :: Form -> IntCPL -> Maybe Bool
evalCPL f (IntCPL tr fa) = case f of
    T                       -> Just True
    V a
        | elem (V a) tr     -> Just True
        | elem (V a) fa     -> Just False
        | otherwise         -> Nothing
    N x
        | isTr x            -> Just False
        | isFa x            -> Just True
        | otherwise         -> Nothing
    C xs
        | anyFa xs          -> Just False
        | anyUn xs          -> Nothing
        | otherwise         -> Just True
    D xs
        | anyTr xs          -> Just True
        | anyUn xs          -> Nothing
        | otherwise         -> Just False
    E x y
        | isUn x || isUn y  -> Nothing
        | sameEval x y      -> Just True
        | otherwise         -> Just False
    where
        isTr     = \x -> evalCPL x (IntCPL tr fa) == Just True
        isFa     = \x -> evalCPL x (IntCPL tr fa) == Just False
        isUn     = \x -> evalCPL x (IntCPL tr fa) == Nothing
        sameEval = \x y -> evalCPL x (IntCPL tr fa) == evalCPL y (IntCPL tr fa)
        anyTr    = any (\x -> evalCPL x (IntCPL tr fa) == Just True)
        anyFa    = any (\x -> evalCPL x (IntCPL tr fa) == Just False)
        anyUn    = any (\x -> evalCPL x (IntCPL tr fa) == Nothing)

-- | Function that checks if a given interpretation is a model for a list of
-- formulas.
modelCheckCPL :: [Form] -> IntCPL -> Bool
modelCheckCPL fs int = all (\x -> evalCPL x int == Just True) fs

-- | Takes a list of formulas (Clark's completion), an interpretation and a list
-- of formulas whose value is unknown, and seeks for the model for all of the
-- formulas. It is not generalised for every type of the formula, because the
-- completion of a logic program contains only formulas of the specific kind,
-- i.e. negated variables and equivalences.
invariants :: [Form] -> (IntCPL, [Form]) -> (IntCPL, [Form])
invariants [] (int, un)             = (int, un)
invariants (f:fs) ((IntCPL tr fa), un)    = case f of
    N a             -> invariants fs ((IntCPL tr (a : fa)), un)
    E a T           -> invariants fs ((IntCPL  (a : tr) fa), un)
    E a b
        | isTr b    -> invariants fs ((IntCPL (a : tr) fa), un)
        | isFa b    -> invariants fs ((IntCPL tr (a : fa)), un)
        | otherwise -> invariants fs (int, f : un)
        where
            int  = (IntCPL tr fa)
            isTr = \x -> evalCPL x (IntCPL tr fa) == Just True
            isFa = \x -> evalCPL x (IntCPL tr fa) == Just False
            isUn = \x -> evalCPL x (IntCPL tr fa) == Nothing

-- | Function that iterates @invariants@ till the set of formulas with unknown
-- value is empty or does not change.
invIter :: [Form] -> (IntCPL, [Form], [Form]) -> (IntCPL, [Form])
invIter fs (int, un, done)
    | null newUn            = (newInt, newUn)
    | eqLists newUn done    = (int, un)
    | otherwise             = invIter newUn (newInt, [], newDone)
        where
            newInt  = fst inv
            newUn   = snd inv
            inv     = invariants fs (int, un)
            newDone = union done newUn

-- | Function that starts from the empty interpretation and searches for the
-- model for the list of formulas. If it does not succeed, then it returns
-- a "part" of the model and the list of formulas with unknown value.
modelSearch :: [Form] -> (IntCPL, [Form])
modelSearch fs = invIter (sort fs) ((IntCPL [] []), [], [])

--------------------------------------------------------------------------------
-- Next step is to write function that creates a model for a list of formulas --
-- even if there are formulas with unknown value.                             --
--------------------------------------------------------------------------------

-- | Function that makes models for a list of formulas where some of them have
-- 'Un' value.
makeModels :: [Form] -> [IntCPL]
makeModels fs = case (modelSearch fs) of
    (int, [])   -> [int]
    (int, uns)  -> sortInts $ foldl1' combineInts $ map makeTr uns
        where
            makeTr = \x -> makeFormTr x int

-- | Sorts a list of interpretations in such a way that the smallest
-- interpretations are at the beginning of the list and the largest at the end.
-- One interpretation in smaller than the other iff the set containing variables
-- mapped to 'true' is smaller.
sortInts :: [IntCPL] -> [IntCPL]
sortInts = sortBy (\(IntCPL a _) (IntCPL b _) -> compare (length a) (length b))

-- | Function that modifies a given interpretation in such a way that the
-- given formula becomes 'true'. The result is a list of interpretations that
-- make the formula 'true'.
-- Note: The assumption is that the formula is evaluated as 'undecided'.
makeFormTr :: Form -> IntCPL -> [IntCPL] 
makeFormTr f (IntCPL tr fa) = case f of
    V a             -> [IntCPL ((V a) : tr) fa]
    N v             -> makeFa v
    C fs            -> foldl1' combineInts $ map makeTr $ getUns fs
    D fs            -> concatMap makeTr $ getUns fs
    E af bf
        | isTr af   -> makeTr bf
        | isFa af   -> makeFa bf
        | isTr bf   -> makeTr af
        | isFa bf   -> makeFa af
        | otherwise -> (combineInts (makeTr af) (makeTr bf)) ++ (combineInts (makeFa af) (makeFa bf))
    where
        int    = (IntCPL tr fa)
        isTr   = \x -> evalCPL x int == Just True
        isFa   = \x -> evalCPL x int == Just False
        isUn   = \x -> evalCPL x int == Nothing
        getUns = \x -> [ y | y <- x, isUn y ]
        makeTr = \x -> makeFormTr x int
        makeFa = \x -> makeFormFa x int
    
-- | Function that modifies a given interpretation in such a way that the
-- given formula becomes 'false'. The result is a list of interpretations that
-- make the formula 'false'.
-- Note: The assumption is that the formula is evaluated as 'undecided'.
makeFormFa :: Form -> IntCPL -> [IntCPL] 
makeFormFa f (IntCPL tr fa) = case f of
    V a             -> [IntCPL tr ((V a) : fa)]
    N v             -> makeTr v
    C fs            -> concatMap makeFa $ getUns fs
    D fs            -> foldl1' combineInts $ map makeFa $ getUns fs
    E af bf
        | isTr af   -> makeFa bf
        | isFa af   -> makeTr bf
        | isTr bf   -> makeFa af
        | isFa bf   -> makeTr af
        | otherwise -> (combineInts (makeTr af) (makeFa bf)) ++ (combineInts (makeFa af) (makeTr bf))
    where
        int    = (IntCPL tr fa)
        isTr   = \x -> evalCPL x int == Just True
        isFa   = \x -> evalCPL x int == Just False
        isUn   = \x -> evalCPL x int == Nothing
        getUns = \x -> [ y | y <- x, isUn y ]
        makeFa = \x -> makeFormFa x int
        makeTr = \x -> makeFormTr x int

-- | Function that reduces given two lists of interpretations to a single list
-- of interpretation, where the resulting list contains combined interpretations
-- from of the two lists (in all possible combinations).
combineInts :: [IntCPL] -> [IntCPL] -> [IntCPL]
combineInts xs ys = stripM $ map combineInt $ allPairs
    where
        allPairs = [ (a, b) | a <- xs, b <- ys ]
        stripM (x:xs) = case x of
            Nothing  -> stripM xs
            Just int -> int : stripM xs

-- | Function that combines two interpretations. Returns 'Nothing' if
-- interpretations are inconsistent.
combineInt :: (IntCPL, IntCPL) -> Maybe IntCPL
combineInt ((IntCPL atr afa), (IntCPL btr bfa))
    | inconsistent = Nothing
    | otherwise    = Just (IntCPL (nub $ atr ++ btr) (nub $ afa ++ bfa))
    where
        inconsistent = jointElem atr bfa || jointElem afa btr

p1a :: LP
p1a = [Cl (A 2 "") [A 1 ""] [A 4 ""], Cl (A 1 "") [A 3 ""] [], Fact (A 5 "")]
