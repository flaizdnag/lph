{-# LANGUAGE MultiParamTypeClasses #-}

{-|
Module      : LogicPrograms
Description : Logic programs with basic properties as Herbrand base or parts of
              clauses.
Copyright   : (c) Aleksandra Cz., 2017-
                  Kinga O., 2017-
                  Andrzej G., 2017-
License     : GPL-3
Maintainer  : andrzej.m.gajda@gmail.com
Stability   : experimental
Portability : POSIX

Logic programs in the propositional case are defined as sets of clauses, which
in turn can be defined in the following way:

\[
a_0 \leftarrow a_1 , ... , a_n , \sim\!a_{n+1} , ... , \sim\!a_m
\]

where \(a_0\), ..., \(a_m\) are propositions called atoms, \(\leftarrow\) is
a simple connector and \(\sim\) represents negation as finite failure.
Atom \(a_0\) is called the head of a clause and the rest of the atoms, i.e.
\(a_1\), ..., \(a_m\), form the body of a clause. As it can be seen in the
example above, there are two types of literals in the body of a clause: atoms
and negated atoms. Since the negated atoms occur only in the body of a clause,
we decided to implement a clause as a tuple with atom (the head of a clause), a
list of atoms that occur without the negation in the body of a clause, and a
list of atoms that occur with negation in the body of a clause, instead of
defining the negation itself.

This module contains definitions of types for atoms, clauses and logic programs,
as well as functions that provide essential properties of clauses and logic
programs.
-}
module LogicPrograms
    ( Atom (..)
    , Clause (..)
    , LP
    , IntLP (..)
    , OverlappingAtoms
    , clPBody
    , clNBody
    , clBodyDup
    , clBody
    , lpHeadsDup
    , lpHeads
    , lpBodiesDup
    , lpBodies
    , onlyHeads
    , onlyBodies
    , lpPBodiesDup
    , lpPBodies
    , lpNBodiesDup
    , lpNBodies
    , bpDup
    , bp
    , atomDef
    , isModel2vLP
    , isModelLukasiewiczLP
    , evalBody2v
    , evalBodyLukasiewicz
    , lpSymDifference
    , modifiedLP
    , intsLPgenerator2v
    , intsLPgeneratorLuk
    , getCounterModelsLuk
    , getCounterModels2v
    , overlappingAtoms
    , bodyLength
    , bodiesLength
    , clSameHeads
    , clsSameHeads
    ) where

import Auxiliary
import TwoValuedSem
import ThreeValuedSem
import Data.List (nub, intercalate, subsequences, intersect, (\\), partition)
import System.Random


-- | Atoms are basic structures for clauses.
data Atom = A { idx :: Int, label :: [Char] }
    deriving (Read)

{-
instance Read Atom where
    read "A"
-}

instance Show Atom where
    show (A idx lab)
        | null lab  = "A" ++ show idx
        | otherwise = "A" ++ show idx ++ "^" ++ lab

instance Eq Atom where
    A a xs == A b ys = a == b && eqLists xs ys

instance Ord Atom where
    A a xs < A b ys = a < b || (a == b && ltLists xs ys)
    
    a <= b = (a < b) || (a == b)
    a >  b = b < a
    a >= b = b <= a

instance TwoValuedSemantic Atom IntLP where
    eval2v a int
        | elem a (trLP int) = Tr2v
        | otherwise         = Fa2v

instance LukasiewiczSemantic Atom IntLP where
    evalLukasiewicz a int
        | elem a (trLP int) = Tr3v
        | elem a (faLP int) = Fa3v
        | otherwise         = Un3v

instance Bounded Atom where
    minBound = A 1 ""
    maxBound = A 1000 ""

instance Enum Atom where
    toEnum i = A i ""
    fromEnum (A i _) = i

instance Random Atom where
    random g = case randomR (fromEnum (minBound :: Atom), fromEnum (maxBound :: Atom)) g of
                 (r, g') -> (toEnum r, g')

    randomR (a,b) g = case randomR (fromEnum a, fromEnum b) g of
                        (r, g') -> (toEnum r, g')


-- | We do not implement negation as finite failure---instead, the negated
-- atoms from the body of a clause are kept in a separate list. The tuple is
-- organised in the following way: atom, which is the head of a clause, list of
-- atoms from the body of a clause that are not preceded by negation, list of
-- atoms from the body of a clause that are negated.
data Clause =
      Fact { clHead :: Atom }
    | Assumption { clHead :: Atom }
    | Cl
        { clHead :: Atom
        , clPAtoms :: [Atom]
        , clNAtoms :: [Atom]
        }
    deriving (Read)

instance TwoValuedSemantic Clause IntLP where
    eval2v cl int = case cl of
        Fact h                 -> eval2v h int
        Assumption _           -> Tr2v
        _
            | headTr || bodyFa -> Tr2v
            | otherwise        -> Fa2v
        where
            headTr = eval2v (clHead cl) int == Tr2v
            bodyFa = evalBody2v cl int == Fa2v

instance LukasiewiczSemantic Clause IntLP where
    evalLukasiewicz cl int = case cl of
        Fact h                 -> evalLukasiewicz h int
        Assumption _           -> Tr3v
        _
            | headTr           -> Tr3v
            | bodyFa           -> Tr3v
            | headUn && bodyUn -> Tr3v
            | headFa && bodyTr -> Fa3v
            | otherwise        -> Un3v
        where
            headTr = evalLukasiewicz (clHead cl) int == Tr3v
            headFa = evalLukasiewicz (clHead cl) int == Fa3v
            headUn = evalLukasiewicz (clHead cl) int == Un3v 
            bodyTr = evalBodyLukasiewicz cl int == Tr3v
            bodyFa = evalBodyLukasiewicz cl int == Fa3v
            bodyUn = evalBodyLukasiewicz cl int == Un3v

instance Eq Clause where
    Fact h1       == Fact h2        = h1 == h2
    Assumption h1 == Assumption h2  = h1 == h2
    Cl h1 pb1 nb1 == Cl h2 pb2 nb2  = h1 == h2 && eqLists pb1 pb2 && eqLists nb1 nb2
    _             == _              = False

instance Show Clause where
    show cl = case cl of
        Fact h          -> show h ++ " <- Top"
        Assumption h    -> show h ++ " <- Bot"
        Cl h pb nb
            | null pb   -> show h ++ " <- ~" ++ intercalate ", ~" (map show nb)
            | null nb   -> show h ++ " <- " ++ intercalate ", " (map show pb)
            | otherwise -> show h ++ " <- " ++ intercalate ", " (map show pb) ++ ", ~" ++ intercalate ", ~" (map show nb)


-- | Logic program is a list of clauses.
type LP = [Clause]


-- | An interpretation is a tuple with lists of atoms: the first list contains
-- atoms that are mapped to truth and the second those that are mapped to
-- false.
data IntLP = IntLP { trLP :: [Atom] , faLP :: [Atom] }
    deriving (Read)

instance Show IntLP where
    show (IntLP tr fa) = "(" ++ show tr ++ ", " ++ show fa ++ ")"

instance Eq IntLP where
    IntLP tr1 fa1 == IntLP tr2 fa2 = eqLists tr1 tr2 && eqLists fa1 fa2

-- | The ordering of interpretations is knowledge-based ordering.
instance Ord IntLP where
    IntLP tr1 fa1 < IntLP tr2 fa2 =
        isProperSublist tr1 tr2 && isProperSublist fa1 fa2

    a <= b = (a < b) || (a == b)
    a >  b = b < a
    a >= b = b <= a

type OverlappingAtoms = (Atom, Atom)


-- | Positive body of a clause, i.e. atoms that are not preceded by negation.
clPBody :: Clause -> [Atom]
clPBody cl = case cl of
    Cl _ _ _     -> clPAtoms cl
    Fact _       -> []
    Assumption _ -> []


-- | Negative body of a clause, i.e. atoms that are preceded by negation.
clNBody :: Clause -> [Atom]
clNBody cl = case cl of
    Cl _ _ _     -> clNAtoms cl
    Fact _       -> []
    Assumption _ -> []


-- | Atoms from the body of a clause (with duplicates).
clBodyDup :: Clause -> [Atom]
clBodyDup cl = case cl of
    Cl _ _ _     -> clPBody cl ++ clNBody cl
    Fact _       -> []
    Assumption _ -> []


-- | Atoms from the body of a clause (without duplicates).
clBody :: Clause -> [Atom]
clBody = nub . clBodyDup


-- | All heads of clauses from a given logic program with duplicates.
lpHeadsDup :: LP -> [Atom]
lpHeadsDup = map clHead


-- | All heads of clauses from a given logic program without duplicates.
lpHeads :: LP -> [Atom]
lpHeads = nub . lpHeadsDup


-- | Atoms from the bodies of all clauses from a given logic program with
-- duplicates.
lpBodiesDup :: LP -> [Atom]
lpBodiesDup = concatMap clBodyDup


-- | Atoms from the bodies of all clauses from a given logic program without
-- duplicates.
lpBodies :: LP -> [Atom]
lpBodies = nub . lpBodiesDup


-- | A list of atoms that are heads of some clauses from a given logic program,
-- and in the same time they do not occur in any body of those clauses.
onlyHeads :: LP -> [Atom]
onlyHeads lp = [ a | a <- lpHeads lp, not $ elem a (lpBodiesDup lp) ]


-- | A list of atoms that occur in a body of some clause from a given logic
-- program, and in the same time they do not occur in any head of those clauses.
onlyBodies :: LP -> [Atom]
onlyBodies lp = [ a | a <- lpBodies lp, not $ elem a (lpHeads lp) ]


-- | Positive atoms from bodies of all clauses form a given logic program with
-- duplicates.
lpPBodiesDup :: LP -> [Atom]
lpPBodiesDup = concatMap clPBody


-- | Positive atoms from bodies of all clauses form a given logic program
-- without duplicates.
lpPBodies :: LP -> [Atom]
lpPBodies = nub . lpPBodiesDup


-- | Negative atoms from bodies of all clauses form a given logic program with
-- duplicates.
lpNBodiesDup :: LP -> [Atom]
lpNBodiesDup = concatMap clNBody


-- | Negative atoms from bodies of all clauses form a given logic program with
-- duplicates.
lpNBodies :: LP -> [Atom]
lpNBodies = nub . lpNBodiesDup


-- | The Herbrand base of a logic program, i.e. the list of all atoms that occur
-- in the clauses from a given logic program (with duplicates).
bpDup :: LP -> [Atom]
bpDup lp = lpHeadsDup lp ++ lpBodiesDup lp


-- | The Herbrand base of a logic program, i.e. the list of all atoms that occur
-- in the clauses from a given logic program (without duplicates).
bp :: LP -> [Atom]
bp = nub . bpDup


-- | Definition of an atom is the set of all clauses that have the atom as the
-- head.
atomDef :: Atom -> LP -> LP
atomDef a lp = [ cl | cl <- lp, clHead cl == a ]


-- | Evaluates the body of a clause in the two-valued semantic.
evalBody2v :: Clause -> IntLP -> TwoValues
evalBody2v cl int = case cl of
    Fact _                           -> Tr2v
    Assumption _                     -> Fa2v
    Cl _ pb nb
        | any isFa pb || any isTr nb -> Fa2v
        | otherwise                  -> Tr2v
    where
        isTr x = eval2v (x :: Atom) int == Tr2v
        isFa x = eval2v (x :: Atom) int == Fa2v


-- | Evaluates the body of a clause in the Lukasiewicz semantic.
evalBodyLukasiewicz :: Clause -> IntLP -> ThreeValues
evalBodyLukasiewicz cl int = case cl of
    Fact _                           -> Tr3v
    Assumption _                     -> Fa3v
    Cl _ pb nb
        | any isFa pb || any isTr nb -> Fa3v
        | any isUn (pb ++ nb)        -> Un3v
        | otherwise                  -> Tr3v
    where
        isTr x = evalLukasiewicz (x :: Atom) int == Tr3v
        isFa x = evalLukasiewicz (x :: Atom) int == Fa3v
        isUn x = evalLukasiewicz (x :: Atom) int == Un3v


-- | Checks if a given interpretation is a model for a given logic program in
-- two-valued semantic.
isModel2vLP :: LP -> IntLP -> Bool
isModel2vLP lp int = all (\x -> eval2v x int == Tr2v) lp


-- | Checks if a given interpretation is a model for a given logic program in
-- Lukasiewicz semantic.
isModelLukasiewiczLP :: LP -> IntLP -> Bool
isModelLukasiewiczLP lp int = all (\x -> evalLukasiewicz x int == Tr3v) lp


-- | Generator of all possible interpretations from a given set of atoms (in
-- 2-valued semantic).
intsLPgenerator2v :: [Atom] -> [IntLP]
intsLPgenerator2v as =
    [ IntLP x (as \\ x) | x <- powerAs ]
    where
        powerAs = subsequences as


-- | Generator of all possible interpretations from a given set of atoms (in
-- Lukasiewicz semantic).
intsLPgeneratorLuk :: [Atom] -> [IntLP]
intsLPgeneratorLuk as =
    [ IntLP x y | x <- powerAs, y <- powerAs, (null $ intersect y x) && (null $ intersect x y) ]
    where
        powerAs = subsequences as


-- | Generates all models of a given logic program that are not models of
-- a given clause (for 2-valued semantic).
getCounterModels2v :: LP -> Clause -> [IntLP]
getCounterModels2v lp cl = filter countermodels (intsLPgenerator2v $ bp $ cl : lp)
    where
        countermodels x = isModel2vLP lp x && (not $ isModel2vLP [cl] x)


-- | Generates all models of a given logic program that are not models of
-- a given clause (for Lukasiewicz semantic).
getCounterModelsLuk :: LP -> Clause -> [IntLP]
getCounterModelsLuk lp cl = filter countermodels (intsLPgeneratorLuk $ bp $ cl : lp)
    where
        countermodels x = isModelLukasiewiczLP lp x && (not $ isModelLukasiewiczLP [cl] x)


-- | Symmetric difference between two logic programs (without atoms with h in
-- the upper index).
lpSymDifference :: LP -> LP -> LP
lpSymDifference lp1 lp2 =
    [ a | a <- symDifference lp1 lp2, not $ elem 'h' (label $ clHead a) ]


-- | Modifies a logic program w.r.t. a clause h, i.e. atoms from the body of
-- h are added to the logic program as facts, if they are not preceded by
-- negation, and as assumptions, if they are preceded by negation.
modifiedLP :: LP -> Clause -> LP
modifiedLP lp cl = case cl of
    Fact _       -> lp
    Assumption _ -> lp
    Cl _ pb nb   -> lp ++ facts ++ assumptions
        where
            makeFact x       = Fact (A (idx x) (label x ++ "h"))
            makeAssumption x = Assumption (A (idx x) (label x ++ "h"))
            facts            = map makeFact pb
            assumptions      = map makeAssumption nb


-- | List of atoms that "overlap", i.e. atoms that have the same index number
-- but one of them has label with "h" and the other does not.
overlappingAtoms :: LP -> [OverlappingAtoms]
overlappingAtoms lp = [ (atom, atomCouterpart) |
    atom <- snd partitionedAtoms,
    atomCouterpart <- fst partitionedAtoms,
    LogicPrograms.idx atom == LogicPrograms.idx atomCouterpart ]
    where
        -- atoms with "h" in the label and atoms without "h" in the label
        partitionedAtoms = partition (\x -> elem 'h' (LogicPrograms.label x)) (bp lp)


-- | The length of the body of a given clause.
bodyLength :: Clause -> Int 
bodyLength = length . clBody


-- | Lengths of all bodies of clauses from a given logic program. 
bodiesLength :: LP -> [Int] 
bodiesLength = map bodyLength


-- | The number of clauses that have the same atom in their head as the given
-- clause.
clSameHeads :: Clause -> LP -> Int 
clSameHeads cl lp = length [ cls | cls <- lp, clHead cls == clHead cl ]


-- | The number of clauses that have the same atom in their head as the given
-- clause for every clause in a given logic program. 
clsSameHeads :: LP -> [Int]
clsSameHeads lp = map (\x -> clSameHeads x lp) lp
