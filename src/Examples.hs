{-|
Module      : Examples
Description : Examples of Horn clauses.
Copyright   : (c) Aleksandra Cz., 2017
                  Kinga O., 2017
                  Andrzej G., 2017
License     : GPL-3
Maintainer  : andrzej.m.gajda@gmail.com
Stability   : experimental
Portability : POSIX

Longer description
-}
module Examples () where

import LogicPrograms


-- from LogicPrograms
lp1 :: LP
lp1 = [Cl (A 2 "") [A 1 ""] [A 4 ""], Cl (A 3 "") [A 1 ""] []]

cl1 :: Clause
cl1 = Cl (A 2 "") [A 1 ""] [A 3 ""]

lp2 :: LP
lp2 = [Cl (A 2 "") [A 1 ""] [A 4 ""], Cl (A 3 "") [A 1 ""] [], Assumption (A 1 "")]

lp3 ::LP
lp3 = [Assumption (A 1 ""), Cl (A 2 "") [A 1 ""] []]

cl2 :: Clause
cl2 = Cl (A 2 "") [A 1 ""] []

lp4 :: LP
lp4 = [Fact (A 1 ""), Cl (A 2 "") [A 1 ""] []]

cl3 :: Clause
cl3 = Cl (A 2 "") [] [A 1 ""]

lp5 :: LP
lp5 = [Cl (A 1 "") [] [A 2 ""], Assumption (A 2 "")]

cl4 :: Clause
cl4 = Fact (A 1 "")

lp6 :: LP
lp6 = [Fact (A 1 ""), Cl (A 2 "") [] [A 1 ""]]

cl5 :: Clause
cl5 = Cl (A 2 "") [] [A 1 "", A 3 ""]

-- from Completion

p1a :: LP
p1a = [Cl (A 2 "") [A 1 ""] [A 4 ""], Cl (A 1 "") [A 3 ""] [], Fact (A 5 "")]

-- from PhiOperator
lp1' :: LP
lp1' = [Cl (A 2 "") [A 1 ""] [A 4 ""], Cl (A 3 "") [A 1 ""] []]

cl1' :: Clause
cl1' = Cl (A 2 "") [A 1 ""] [A 3 ""]

lp2' :: LP
lp2' = [Cl (A 2 "") [A 1 ""] [A 4 ""], Cl (A 3 "") [A 1 ""] [], Assumption (A 1 "")]

lp3' ::LP
lp3' = [Assumption (A 1 ""), Cl (A 2 "") [A 1 ""] []]

cl2' :: Clause
cl2' = Cl (A 2 "") [A 1 ""] []

lp4' :: LP
lp4' = [Fact (A 1 ""), Cl (A 2 "") [A 1 ""] []]

cl3' :: Clause
cl3' = Cl (A 2 "") [] [A 1 ""]

cl4' :: Clause
cl4' = Cl (A 2 "") [] [A 1 "", A 3 ""]


p3 :: LP
p3 = [Cl (A 1 "") [A 2 ""] [], Cl (A 1 "") [A 3 ""] [], Assumption (A 3 "")]

p4 :: LP
p4 = [Fact (A 3 ""), Assumption (A 3 "")]

p5 :: LP
p5 = [Cl (A 1 "") [A 2 ""] [A 3 ""], Assumption (A 3 ""), Fact (A 2 "")]

p6 :: LP
p6 = [Cl (A 1 "") [A 2 ""] []]





exl = [(A 1, [A 2], [A 3]), (A 4, [A 6], [A 5]), (A 4, [A 1], [A 7]), (A 2, [], [])]
ex1 = [(A 1, [], []), (A 2, [], [A 3]), (A 4, [A 1], [A 3])]
ex1a = [(A 1, [], []), (A 2, [], [A 3]), (A 4, [A 1, A 3], [])]
ex2 = [(A 1, [A 3, A 5], [A 2, A 7]), (A 8, [A 9], [A 10, A 11]), (A 12, [A 4, A 6], [A 13, A 11])]
ex3 = [(A 1, [A 3, A 5], [A 2, A 7]), (A 8, [A 9], [A 10, A 11]), (A 1, [A 4, A 6], [A 13, A 11])]
ex4 = [(A 1, [A 3, A 5], [A 2, A 7]), (A 8, [A 9], [A 10, A 11]), (A 12, [A 4, A 6], [A 13, A 11]), (A 1, [A 3, A 5], [A 2, A 7]), (A 8, [A 9], [A 10, A 11]), (A 12, [A 4, A 6], [A 13, A 11])]
ex5 = [(A 1, [], []), (A 2, [], [A 3]), (A 4, [A 1], [A 3]), (A 2, [A 1], [A 3]), (A 3, [A 12], [A 33]), (A 3, [A 1], [A 3]), (A 1, [A 31], [A 3])]
ex6 = [(A 4, [A 5], []), (A 1, [], [A 2, A 3]), (A 2, [], [A 4])]
ex7 = [(A 4, [A 5], []), (A 12, [], []), (A 1, [], [A 2, A 3]), (A 2, [], [A 4]), (A 6, [], [])]
ex8 = [(A 10, [], []), (A 4, [A 5], []), (A 12, [], []), (A 1, [], [A 2, A 3]), (A 2, [], [A 4]), (A 6, [], [])]
ex9 = [(A 10, [], []), (A 4, [A 5], []), (A 4, [A 19], [A 11]), (A 12, [], []), (A 1, [], [A 2, A 3]), (A 2, [], [A 4]), (A 6, [], [])]
ex9a = [(A 1, [], []), (A 3, [], [A 1]), (A 3, [], [])]
-- no T
ex10 = [(A 10,[],[A 19]),(A 4,[A 5],[]),(A 12,[],[A 22]),(A 1,[],[A 2,A 3]),(A 2,[],[A 4]),(A 6,[],[A 18])]
-- no N
ex11 = [(A 10,[],[A 4]),(A 4,[A 12],[]),(A 12,[],[]),(A 2,[],[A 2,A 4]),(A 6,[],[A 10])]

--loop
--ex3 = [(A 1, [A 3], []), (A 2, [A 1], [A 4]), (A 3, [], [A 2])]


--program
p1 = [(A 1, [A 2], [A 3]), (A 1, [], [A 4]), (A 2, [A 5], []), (A 5, [], [])]
--compP, logicP'

{-
trueE (E (V (A 1)) (D [C [V (A 2),N (V (A 3))]])) ([V (A 5)],[V (A 3),V (A 4)])
interpretation [[E (V (A 1)) (D [C [V (A 2),N (V (A 3))],N (V (A 4))]),E (V (A 2)) (V (A 5))],[E (V (A 5)) T],[N (V (A 3)),N (V (A 4))]]
interpretation (groupByValue (compP p1))
-}
