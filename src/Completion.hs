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
    ( Form (..)
    , IntCPL
    , Bool3
    , comp
    , modelCheckCPL
    , modelSearch
    , makeModels
    , intLPTointCPL
    ) where

import Formulas
import Auxiliary
import Data.List

-- | The CPL language.
data Form = V Atom 
          | N Form        -- negation
          | C [Form]      -- conjunction 
          | D [Form]      -- disjunction 
          | E Form Form   -- equivalence
          | T             -- verum
    deriving (Show)

instance Eq Form where
    T     == T      = True
    T     == _      = False
    _     == T      = False
    V a   == V b    = a == b
    V _   == _      = False
    _     == V _    = False
    N x   == N y    = x == y
    N _   == _      = False
    _     == N _    = False
    C xs  == C ys   = eqLists xs ys
    C _   == _      = False
    _     == C _    = False
    D xs  == D ys   = eqLists xs ys
    D _   == _      = False
    _     == D _    = False
    E a b == E c d  = (a == c && b == d) || (a == d && b == c)

instance Ord Form where
    T     < _       = True
    V a   < V b     = a < b
    V _   < _       = True
    _     < V _     = False
    N x   < N y     = x < y
    N _   < _       = True
    _     < N _     = False
    C xs  < C ys    = sort xs < sort ys
    C _   < _       = True
    _     < C _     = False
    D xs  < D ys    = sort xs < sort ys
    D _   < _       = True
    _     < D _     = False
    E a b < E c d   = (a < c) -- should be enough...

    a <= b = (a < b) || (a == b)
    a >  b = b < a
    a >= b = b <= a

-- | An interpretation is a tuple with lists of variables: the first list
-- contains variables that are mapped to 'truth' and the second those that are
-- mapped to 'false'.
type IntCPL = ([Form], [Form])

-- | Data type used in the functions that seek for the model of the completion
-- of a logic program. We use three values to evaluate formulas of the classical
-- propositional logic, however this is a different approach than the standard
-- one given by Łukasiewicz, what can be seen in the @eval@ function.
data Bool3 = Fa | Tr | Un
    deriving (Show, Eq)

-- | Converts an atom into a variable.
atomToVar :: Atom -> Form
atomToVar a = V a

-- | Converts a list of atoms into variables.
atomsToVar :: [Atom] -> [Form]
atomsToVar = map atomToVar

-- | Converts an atom into a negated variable.
atomToNVar :: Atom -> Form
atomToNVar a = N (V a)

-- | Converts a list of atoms into negated variables.
atomsToNVar :: [Atom] -> [Form]
atomsToNVar = map atomToNVar

-- | Creates an interpretation for CPL from an interpretation for LP.
intLPtointCPL :: IntLP -> IntCPL
intLPtointCPL (tr, fa) = (atomsToVar tr, atomsToVar fa)

-- | Converts atoms from the body of a Horn clause into a conjunction, i.e.
-- a tuple where the first element is the head of the Horn clause and the second
-- is the conjunction of atoms converted into variables. If the body of the Horn
-- clause contains one atom, then the conjunction is not created (the atom is
-- only converted to variable). If the body of the Horn clause does not contain
-- any atoms, then verum (T) is inserted as the second element of the tuple.
addC :: HClause -> (Form, Form)
addC (h, [], [])        = (V h, T)
addC (h, [], nb) 
    | length nb == 1    = (V h, N (V (head nb)))
    | otherwise         = (V h, C (atomsToNVar nb))
addC (h, pb, []) 
    | length pb == 1    = (V h, V (head pb))
    | otherwise         = (V h, C (atomsToVar pb))
addC (h, ys, xs)        = (V h, C (atomsToVar ys ++ atomsToNVar xs))

-- | Adding disjunction and equivalence to a conjunction.
addDE :: [(Form, Form)] -> Form
addDE c
    | length c > 1  = E (fst (head c)) (D (map snd c))
    | otherwise     = E (fst (head c)) (snd (head c))

-- | Creating equivalences with disjunctions for the whole logic program.
addDElp :: LogicP -> [Form]
addDElp = map addDE . preGrouped
    where
        preGrouped = groupBy (\x y -> fst x == fst y) . map addC . sort

-- | Adding negated variables made from atoms that do not appear as heads in the
-- logic program.
addN :: LogicP -> [Form]
addN lp = atomsToNVar ((bP lp) \\ (bPHeads lp))

-- | Creating Clark's completion for a logic program.
comp :: LogicP -> [Form]
comp lp = addDElp lp ++ addN lp

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
evalCPL :: Form -> IntCPL -> Bool3
evalCPL f (tr, fa) = case f of
    T                       -> Tr
    V a
        | elem (V a) tr     -> Tr
        | elem (V a) fa     -> Fa
        | otherwise         -> Un
    N x
        | isTr x            -> Fa
        | isFa x            -> Tr
        | otherwise         -> Un
    C xs
        | anyFa xs          -> Fa
        | anyUn xs          -> Un
        | otherwise         -> Tr
    D xs
        | anyTr xs          -> Tr
        | anyUn xs          -> Un
        | otherwise         -> Fa
    E x y
        | isUn x || isUn y  -> Un
        | sameEval x y      -> Tr
        | otherwise         -> Fa
    where
        isTr     = \x -> evalCPL x (tr, fa) == Tr
        isFa     = \x -> evalCPL x (tr, fa) == Fa
        isUn     = \x -> evalCPL x (tr, fa) == Un
        sameEval = \x y -> evalCPL x (tr, fa) == evalCPL y (tr, fa)
        anyTr    = any (\x -> evalCPL x (tr, fa) == Tr)
        anyFa    = any (\x -> evalCPL x (tr, fa) == Fa)
        anyUn    = any (\x -> evalCPL x (tr, fa) == Un)

-- | Function that checks if a given interpretation is a model for a list of
-- formulas.
modelCheckCPL :: [Form] -> IntCPL -> Bool
modelCheckCPL fs int = all (\x -> evalCPL x int == Tr) fs

-- | Takes a list of formulas (Clark's completion), an interpretation and a list
-- of formulas whose value is unknown, and seeks for the model for all of the
-- formulas. It is not generalised for every type of the formula, because the
-- completion of a logic program contains only formulas of the specific kind,
-- i.e. negated variables and equivalences.
invariants :: [Form] -> (IntCPL, [Form]) -> (IntCPL, [Form])
invariants [] (int, un)             = (int, un)
invariants (f:fs) ((tr, fa), un)    = case f of
    N a             -> invariants fs ((tr, a : fa), un)
    E a T           -> invariants fs ((a : tr, fa), un)
    E a b
        | isTr b    -> invariants fs ((a : tr, fa), un)
        | isFa b    -> invariants fs ((tr, a : fa), un)
        | otherwise -> invariants fs (int, f : un)
        where
            int  = (tr, fa)
            isTr = \x -> evalCPL x (tr, fa) == Tr
            isFa = \x -> evalCPL x (tr, fa) == Fa
            isUn = \x -> evalCPL x (tr, fa) == Un

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
modelSearch fs = invIter (sort fs) (([], []), [], [])

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
sortInts = sortBy (\(a, _) (b, _) -> compare (length a) (length b))

-- | Function that modifies a given interpretation in such a way that the
-- given formula becomes 'true'. The result is a list of interpretations that
-- make the formula 'true'.
-- Note: The assumption is that the formula is evaluated as 'undecided'.
makeFormTr :: Form -> IntCPL -> [IntCPL] 
makeFormTr f (tr, fa) = case f of
    V a             -> [((V a) : tr, fa)]
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
        int    = (tr, fa)
        isTr   = \x -> evalCPL x int == Tr
        isFa   = \x -> evalCPL x int == Fa
        isUn   = \x -> evalCPL x int == Un
        getUns = \x -> [ y | y <- x, isUn y ]
        makeTr = \x -> makeFormTr x int
        makeFa = \x -> makeFormFa x int
    
-- | Function that modifies a given interpretation in such a way that the
-- given formula becomes 'false'. The result is a list of interpretations that
-- make the formula 'false'.
-- Note: The assumption is that the formula is evaluated as 'undecided'.
makeFormFa :: Form -> IntCPL -> [IntCPL] 
makeFormFa f (tr, fa) = case f of
    V a             -> [(tr, (V a) : fa)]
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
        int    = (tr, fa)
        isTr   = \x -> evalCPL x int == Tr
        isFa   = \x -> evalCPL x int == Fa
        isUn   = \x -> evalCPL x int == Un
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
combineInt ((atr, afa), (btr, bfa))
    | inconsistent = Nothing
    | otherwise    = Just (nub $ atr ++ btr, nub $ afa ++ bfa)
    where
        inconsistent = jointElem atr bfa || jointElem afa btr

-- | Converts an interpretation for a logic program into an interpretation for
-- CPL.
intLPTointCPL :: IntLP -> IntCPL
intLPTointCPL (tr, fa) = (atomsToVar tr, atomsToVar fa)
--------------------------------------------------------------------------------
-- Older stuff (soon will disappear)                                          --
--------------------------------------------------------------------------------

{-
-- | Removes 'Just' and 'Nothing' from the list of interpretations.
stripM :: [Maybe IntCPL] -> [IntCPL]
stripM []     = []
stripM (x:xs) = case x of
    Nothing  -> stripM xs
    Just int -> int : stripM xs

-- | Function that takes an equivalence with the 'unknown' value and an
-- interpretation, and changes the interpretation in such a way that the
-- equivalence becomes 'true'. In order to maintain "minimality" of the
-- interpretation (understood as the minimal number of atoms mapped to 'true')
-- the function tries to make atoms 'false' in the first place.
-- Note: the result may contain duplicates in both lists.
makeETr :: Form -> IntCPL -> Maybe IntCPL
makeETr f (tr, fa) = case f of
    E a (V b)                                        -- if 'V b' is 'Tr', then 'a' should be 'Tr';
        | isTr (V b)    -> Just (a : tr, fa)         -- if 'a' is 'Tr', then 'V b' should be 'Tr';
        | isTr a        -> Just ((V b) : tr, fa)     -- otherwise 'V b == a', 'V b' and 'a' is 'Un'
        | otherwise     -> Just (tr, a : (V b) : fa) -- or 'V b' (or 'a') is 'Fa', where in all
                                                     -- those cases we want to make
                                                     -- 'a' and 'V b' 'Fa'
                                                     
    E a (N b)                                        -- the negation case is similar
        | isTr (N b)    -> Just (a : tr, fa)         -- as the previous one
        | isTr a        -> Just (tr, b : fa)
        | otherwise     -> Just (b : tr, a : fa)

    E a (C as)
        | isTr (C as)   -> Just (a : tr, fa)
        | isFa (C as)   -> Just (tr, a : fa)
        | isTr a        -> Just (getUns (C as) ++ tr, fa)
        | isFa a        -> Just (tr, getUns (C as) ++ fa)
        | getUns (C as) == [N a]        -> Nothing
        | elem a (getUns (C as))        -> Just (tr, a : fa)
        | otherwise                     -> Just (tr, a : (dif a as) : fa)
    
    --E a (D as)  -> 
    where
        isTr   = \x -> evalCPL x (tr, fa) == Tr
        isFa   = \x -> evalCPL x (tr, fa) == Fa
        isUn   = \x -> evalCPL x (tr, fa) == Un
        getUns = \x -> [ y | y <- breakFormLit x, isUn y ]
        dif    = \x ys -> head . breakFormA $ head (filter (\z -> z /= x && z /= N x) ys) -- this has to be changed

-- | Function that 'breaks' complex formulas to literals, i.e. returns the list
-- of variables and negated variables from the formula.
breakFormLit :: Form -> [Form]
breakFormLit a = case a of
    V b   -> [V b]
    N b   -> [N b]
    C fs  -> fs
    D fs  -> concatMap breakFormLit fs
    E b c -> (breakFormLit b) ++ (breakFormLit c)
    T     -> []

-- | Function that 'breaks' complex formulas to atoms, i.e. returns the list
-- of variables from the formula.
breakFormA :: Form -> [Form]
breakFormA a = case a of
    V b   -> [V b]
    N b   -> [b]
    C fs  -> concatMap breakFormA fs
    D fs  -> concatMap breakFormA fs
    E b c -> (breakFormA b) ++ (breakFormA c)
    T     -> []

-- | Function that makes a model for the completion of a logic program, if there
-- are still formulas with 'unknown' value after @modelSearch@.
makeModel :: [Form] -> (IntCPL, [Form]) -> IntCPL
makeModel fs (int, un) = 

-- groups Formulas by their values [[Unknown], [True], [False]]
groupByValue :: [Form] -> [[Form]]
groupByValue xs = groupBy (\x y -> (invariants x) == (invariants y)) xs


breakForms :: [Form] -> [Form]
breakForms []     = []
breakForms (x:xs) = breakForm x ++ breakForms xs

--generates interp from formulas with known value
interp :: LogicP -> ([Form], [Form])
interp xs = (true, false)
    where
        true  = [a | x <- (groupByValue (compP xs)), invariants (head x) == Tr, a <- (breakForms x)]
        false = [b | x <- (groupByValue (compP xs)), invariants (head x) == Fa, b <- (breakForms x)]

-- returns list of formulas with unknown value
-- TODO Why not use 'groupByValue'?
unE :: [Form] -> [Form]
unE []                        = []
unE (x:xs) 
         | invariants x == Un = x: unE xs
         | otherwise          = unE xs

--checks if conjunction is True/False/Unknown
trueC :: Form -> ([Form], [Form]) -> [Bool3]
trueC (C []) _     = []
trueC (C (a:as)) x = case a of
    N a -> if elem a (fst x) then Fa : (trueC (C as) x)
           else 
               if elem a (snd x) then Tr : (trueC (C as) x)
               else Un : (trueC (C as) x)
    a   -> if elem a (snd x) then Fa : (trueC (C as) x)
           else
               if elem a (fst x) then Tr : (trueC (C as) x)
               else Un : (trueC (C as) x)


trueC' :: Form -> ([Form], [Form]) -> Bool3
trueC' a x 
         | all (Tr==) (trueC a x) = Tr
         | any (Fa==) (trueC a x) = Fa
         | otherwise              = Un

-- checks if disjunction is True/False/Unknown
-- TODO What with 'T'? Cases have to be based on the whole list, i.e. 'a:as'?
trueD :: Form -> ([Form], [Form]) -> [Bool3]
trueD (D []) _ = []
trueD (D (a:as)) x  = case (a:as) of
                    (V b):bs -> if elem (V b) (fst x) then Tr : (trueD (D as) x)
                                else
                                    if elem (V b) (snd x) then Fa : (trueD (D as) x)
                                    else Un : (trueD (D as) x)
                    (N b):bs -> if elem b (snd x) then Tr : (trueD (D as) x) 
                                else
                                    if elem b (fst x) then Fa : (trueD (D as) x)
                                    else Un : (trueD (D as) x)
                    (C b):bs -> (trueC' (C b) x) : (trueD (D as) x)

trueD' :: Form -> ([Form], [Form]) -> Bool3
trueD' a x 
         | any (Tr==) (trueD a x) = Tr
         | all (Fa==) (trueD a x) = Fa
         | otherwise              = Un



-- checks if equivalence is True/False/Unknown
trueE :: Form -> ([Form], [Form]) -> Bool3
trueE (E a b) x = case b of 
                       V c -> if elem (V c) (fst x) then Tr 
                              else
                                  if elem (V c) (snd x) then Fa else Un 
                       N c -> if elem c (snd x) then Tr
                              else
                                  if elem c (fst x) then Fa else Un
                       C c -> trueC' (C c) x
                       D c -> trueD' (D c) x

-- adds heads from equivalences with known value
addHeadToI :: Form -> ([Form], [Form]) -> ([Form], [Form])
addHeadToI (E a b) (c, d) 
    | (not (elem a c)) && ((trueE (E a b) (c, d)) == Tr) = (a : c, d)
    | (not (elem a d)) && ((trueE (E a b) (c, d)) == Fa) = (c, a : d)
    | otherwise                                          = (c, d) -- add from perms

-- takes 3 arguments:
-- -list of equivalences with unknown values
-- -list with permutations of all atoms unused in interpretation 
-- -current interpretation
-- it checks if with current interpretation we can decide values 
-- of other atoms, in result returns actualized interpretation
accI :: [Form] -> [[Form]] -> ([Form], [Form]) -> ([Form], [Form])
accI [] _ (a, b) = (a, b)
accI _ [] (a, b) = (a, b)                
accI (x:xs) (y:ys) (a, b) 
    | (trueE x ((y ++ a), b)) == Un = accI (xs ++ [x]) (y:ys) (a, b)
    | otherwise                     = accI xs (ys) (addHeadToI x (a, b))

-- generalizes type of accInv to make it easier to use in other functions
iterI :: LogicP -> ([Form], [Form])
iterI x = accI (unE (compP x)) (perms x (interp x)) (interp x)

-- checks if generated interpretation lets us establish value of
-- chosen equivalence categorized earlier as unknown
checkUn :: LogicP -> Int -> Bool3
checkUn x n = (trueE (unE (compP x) !! n) (iterI x))

-- gives a list of the values for all the equivalences 
-- that we are able to establish with generated interpretation
check :: LogicP -> Int -> [Bool3]
check x (-1) = []
check x n = (checkUn x n) : check x (n-1)

-- checks if list given by function check contains any
-- 'Un' value
-- if not - returns generated interpretation
-- if it does - returns empty interpretation
check' :: LogicP -> ([Form], [Form])
check' x = if (notElem Un (check x n)) then iterI x
                else ([],[])
                   where
                       n = (length (unE (compP x))) - 1

-- adds negation to formulas
addNToForm :: [Form] -> [Form]
addNToForm []     = []
addNToForm (x:xs) = N x : addNToForm xs

-- creates a list of all Forms from the LogicP that aren't included in I
unAtoms :: LogicP -> ([Form], [Form]) -> [Form]
unAtoms [] _ = []
unAtoms x y  = (a \\ (b ++ f)) ++ (c \\ (d ++ e)) 
                where 
                        a = nub (atomsToForm (bPHeads x) ++ atomsToForm (bPBodiesP x))
                        b = fst y
                        c = (addN (negP x))
                        d = addNToForm (snd y)
                        e = addNToForm (fst y) 
                        f = snd y

-- creates list of all permutations of Formulas we can add to I
perms :: LogicP -> ([Form], [Form]) -> [[Form]]
perms x y = sortWith length $ subsequences (unAtoms x y)
-}
