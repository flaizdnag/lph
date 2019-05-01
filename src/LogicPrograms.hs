{-# LANGUAGE MultiParamTypeClasses #-}

{-|
Module      : LogicPrograms
Description : Logic programs with basic properties as Herbrand base or parts of
              Horn clauses.
Copyright   : (c) Aleksandra Cz., 2017-
                  Kinga O., 2017-
                  Andrzej G., 2017-
License     : GPL-3
Maintainer  : andrzej.m.gajda@gmail.com
Stability   : experimental
Portability : POSIX

Logic programs in the propositional case are defined as sets of Horn clauses,
which in turn can be defined in the following way:

\[
a_0 \leftarrow a_1 , ... , a_n , \sim\!a_{n+1} , ... , \sim\!a_m
\]

where \(a_0\), ..., \(a_m\) are propositions called atoms, \(\leftarrow\) is
a simple connector and \(\sim\) represents negation as finite failure.
Atom \(a_0\) is called the head of a Horn clause and the rest of the atoms,
i.e. \(a_1\), ..., \(a_m\), form the body of a Horn clause. As it can be seen in
the example above, there are two types of literals in the body of a Horn clause:
atoms and negated atoms. Since the negated atoms occur only in the body of a
Horn clause, we decided to implement a Horn clause as a tuple with atom (the
head of a Horn clause), a list of atoms that occur without the negation in the
body of a Horn clause, and a list of atoms that occur with negation in the body
of a Horn clause, instead of defining the negation itself.

This module contains definitions of types for atoms, Horn clauses and logic
programs, as well as functions that provide essential properties of Horn clauses
and logic programs.
-}
module LogicPrograms
    ( Atom (..)
    , Clause (..)
    , LP
    , IntLP (..)
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
    , isModel3vLP
    , evalBody
    , lpSymDifference
    ) where

import Auxiliary
import TwoValuedSem
import ThreeValuedSem
import Data.List (nub, intercalate)


-- | Atoms are basic structures for Horn clauses.
data Atom = A
    { idx :: Int
    , label :: [Char]
    }
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

instance ThreeValuedSemantic Atom IntLP where
    eval3v a int
        | elem a (trLP int) = Tr3v
        | elem a (faLP int) = Fa3v
        | otherwise         = Un3v


-- | We do not implement negation as finite failure---instead, the negated
-- atoms from the body of a Horn clause are kept in a separate list. The tuple
-- is organised in the following way: atom, which is the head of a Horn clause,
-- list of atoms from the body of a Horn clause that are not preceded by
-- negation, list of atoms from the body of a Horn clause that are negated.
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
        Fact h                   -> eval2v h int
        Assumption _             -> Tr2v
        Cl h pb nb
            | isATr h            -> Tr2v
            | isBodyFa pb nb     -> Tr2v
            | otherwise          -> Fa2v
        where
            isATr = \x -> eval2v (x :: Atom) int == Tr2v
            isAFa = \x -> eval2v (x :: Atom) int == Fa2v
            isBodyFa = \x y -> any isAFa (x :: [Atom]) || any isAFa (y :: [Atom])

instance ThreeValuedSemantic Clause IntLP where
    eval3v cl int = case cl of
        Fact h                          -> eval3v h int
        Assumption _                    -> Tr3v
        Cl h pb nb
            | isATr h                   -> Tr3v
            | isBodyFa pb nb            -> Tr3v
            | isAUn h && isBodyUn pb nb -> Tr3v
            | isAFa h && isBodyTr pb nb -> Fa3v
            | otherwise                 -> Un3v
        where
            isATr = \x -> eval3v (x :: Atom) int == Tr3v
            isAFa = \x -> eval3v (x :: Atom) int == Fa3v
            isAUn = \x -> eval3v (x :: Atom) int == Un3v
            isBodyTr = \x y -> all isATr (x :: [Atom]) && all isATr (y :: [Atom])
            isBodyFa = \x y -> any isAFa (x :: [Atom]) || any isAFa (y :: [Atom])
            isBodyUn = \x y -> any isAUn (x :: [Atom]) || any isAUn (y :: [Atom])

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


-- | Logic program is a list of Horn clauses.
type LP = [Clause]


-- | An interpretation is a tuple with lists of atoms: the first list contains
-- atoms that are mapped to 'truth' and the second those that are mapped to
-- 'false'.
data IntLP = IntLP { trLP :: [Atom] , faLP :: [Atom] }
    deriving (Read, Eq)

instance Show IntLP where
    show (IntLP tr fa) = "(" ++ show tr ++ ", " ++ show fa ++ ")"


clPBody :: Clause -> [Atom]
clPBody cl = case cl of
    Cl _ _ _     -> clPAtoms cl
    Fact _       -> []
    Assumption _ -> []

clNBody :: Clause -> [Atom]
clNBody cl = case cl of
    Cl _ _ _     -> clNAtoms cl
    Fact _       -> []
    Assumption _ -> []

clBodyDup :: Clause -> [Atom]
clBodyDup cl = case cl of
    Cl _ _ _     -> clPBody cl ++ clNBody cl
    Fact _       -> []
    Assumption _ -> []

clBody :: Clause -> [Atom]
clBody = nub . clBodyDup

-- | Function that returns all heads of Horn clauses from a given logic program
-- with duplicates.
lpHeadsDup :: LP -> [Atom]
lpHeadsDup = map clHead

-- | Function that returns all heads of Horn clauses from a given logic program
-- without duplicates.
lpHeads :: LP -> [Atom]
lpHeads = nub . lpHeadsDup

-- | Function that returns atoms from the bodies of all Horn clauses from
-- a given logic program with duplicates.
lpBodiesDup :: LP -> [Atom]
lpBodiesDup = concatMap clBodyDup

-- | Function that returns atoms from the bodies of all Horn clauses from
-- a given logic program without duplicates.
lpBodies :: LP -> [Atom]
lpBodies = nub . lpBodiesDup

-- | Function that returns a list of atoms that are heads of some Horn clauses
-- from a given logic program, and in the same time they do not occur in any
-- body of those Horn clauses.
onlyHeads :: LP -> [Atom]
onlyHeads lp = [ a | a <- lpHeads lp, not $ elem a (lpBodiesDup lp) ]

-- | Function that returns a list of atoms that occur in a body of some Horn
-- clause from a given logic program, and in the same time they do not occur in
-- any head of those Horn clauses.
onlyBodies :: LP -> [Atom]
onlyBodies lp = [ a | a <- lpBodies lp, not $ elem a (lpHeads lp) ]

-- | Function that returns positive atoms from bodies of all Horn clauses form
-- a given logic program with duplicates.
lpPBodiesDup :: LP -> [Atom]
lpPBodiesDup = concatMap clPBody

-- | Function that returns positive atoms from bodies of all Horn clauses form
-- a given logic program without duplicates.
lpPBodies :: LP -> [Atom]
lpPBodies = nub . lpPBodiesDup

-- | Function that returns negative atoms from bodies of all Horn clauses form
-- a given logic program with duplicates.
lpNBodiesDup :: LP -> [Atom]
lpNBodiesDup = concatMap clNBody

-- | Function that returns negative atoms from bodies of all Horn clauses form
-- a given logic program with duplicates.
lpNBodies :: LP -> [Atom]
lpNBodies = nub . lpNBodiesDup

-- | Function that returns the Herbrand base of a logic program, i.e. the list
-- of all atoms that occur in the Horn clauses from a given logic program with
-- duplicates.
bpDup :: LP -> [Atom]
bpDup lp = lpHeadsDup lp ++ lpBodiesDup lp

-- | Function that returns the Herbrand base of a logic program, i.e. the list
-- of all atoms that occur in the Horn clauses from a given logic program
-- without duplicates.
bp :: LP -> [Atom]
bp = nub . bpDup

-- | Definition of an atom is the set of all Horn clauses that have the atom as
-- the head.
atomDef :: Atom -> LP -> LP
atomDef a lp = [ cl | cl <- lp, clHead cl == a ]

evalBody :: Clause -> IntLP -> ThreeValues
evalBody cl int = case cl of
    Fact _                           -> Tr3v
    Assumption _                     -> Fa3v
    Cl h pb nb
        | any isFa pb || any isTr nb -> Fa3v
        | any isUn (pb ++ nb)        -> Un3v
        | otherwise                  -> Tr3v
            where
                isTr = \x -> eval3v x int == Tr3v
                isFa = \x -> eval3v x int == Fa3v
                isUn = \x -> eval3v x int == Un3v

-- | Checks if a given interpretation is a model for a given logic program.
isModel2vLP :: LP -> IntLP -> Bool
isModel2vLP lp int = all (\x -> eval2v x int == Tr2v) lp

-- | Checks if a given interpretation is a model for a given logic program.
isModel3vLP :: LP -> IntLP -> Bool
isModel3vLP lp int = all (\x -> eval3v x int == Tr3v) lp

-- | Symmetric difference between two logic programs (without atoms with 'h' in
-- the upper index).
lpSymDifference :: LP -> LP -> LP
lpSymDifference lp1 lp2 =
    [ a | a <- symDifference lp1 lp2, not $ elem 'h' (label $ clHead a) ]

-- | Modifies a logic program w.r.t. a clause 'h', i.e. atoms from the body of
-- 'h' are added to the logic program as facts, if they are not preceded by
-- negation, and as assumptions, if they are preceded by negation.
modifiedLP :: LP -> Clause -> LP
modifiedLP lp cl = case cl of
    Fact _       -> lp
    Assumption _ -> lp
    Cl h pb nb   -> lp ++ facts ++ assumptions
        where
            makeFact x       = Fact (A (idx x) (label x ++ "h"))
            makeAssumption x = Assumption (A (idx x) (label x ++ "h"))
            filteredPB       = filter (\x -> not $ elem (Assumption x) lp) pb
            filteredNB       = filter (\x -> not $ elem (Fact x) lp) nb
            facts            = map makeFact filteredPB
            assumptions      = map makeAssumption filteredNB
