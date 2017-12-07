module CPL where

import FormulasL
import CCompletion

data Form =   V Atom 
            | N Form        -- negation
            | C [Form]      -- conjunction - C Form Form? 
            | D [Form]      -- disjunction - D Form Form? 
            | E Form Form   -- equivalence
            deriving Show

-- changes negative atoms to N Form
addN :: [Atom] -> [Form]
addN x = case x of 
              []     -> []
              (y:ys) -> N (V y) : addN ys

-- changes positive atoms to V Atom
atomToForm :: [Atom] -> [Form]
atomToForm x = case x of
                    []     -> []
                    (y:ys) -> V y : atomToForm ys

-- connects elements of the horn clauses body by conjunction
bodyToC :: HClause -> Form
bodyToC x = case x of 
{-
                 (_, [], neg)  -> case neg of
                                       n -> N (V n)
                                       (n:ns) -> C (addN (n:ns))
                 (_, pos, [])  -> case pos of
                                       p -> V p
                                       (p:ps) -> C (atomToForm (p:ps)) ??? -}
                 (_, pos, neg) -> C (atomToForm pos ++ addN neg)

-- connects head and body of horn clause by equivalence
hClToEq :: HClause -> Form
hClToEq x = case x of 
                 (h, _, _) -> E (V h) (bodyToC x) 