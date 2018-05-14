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

import Formulas

ex1 = [(A 1, [], []), (A 2, [], [A 3]), (A 4, [A 1], [A 3])]
ex2 = [(A 1, [A 3, A 5], [A 2, A 7]), (A 8, [A 9], [A 10, A 11]), (A 12, [A 4, A 6], [A 13, A 11])]
ex3 = [(A 1, [A 3, A 5], [A 2, A 7]), (A 8, [A 9], [A 10, A 11]), (A 1, [A 4, A 6], [A 13, A 11])]
ex4 = [(A 1, [A 3, A 5], [A 2, A 7]), (A 8, [A 9], [A 10, A 11]), (A 12, [A 4, A 6], [A 13, A 11]), (A 1, [A 3, A 5], [A 2, A 7]), (A 8, [A 9], [A 10, A 11]), (A 12, [A 4, A 6], [A 13, A 11])]
ex5 = [(A 1, [], []), (A 2, [], [A 3]), (A 4, [A 1], [A 3]), (A 2, [A 1], [A 3]), (A 3, [A 12], [A 33]), (A 3, [A 1], [A 3]), (A 1, [A 31], [A 3])]
ex6 = [(A 4, [A 5], []), (A 1, [], [A 2, A 3]), (A 2, [], [A 4])]
ex7 = [(A 4, [A 5], []), (A 12, [], []), (A 1, [], [A 2, A 3]), (A 2, [], [A 4]), (A 6, [], [])]
ex8 = [(A 10, [], []), (A 4, [A 5], []), (A 12, [], []), (A 1, [], [A 2, A 3]), (A 2, [], [A 4]), (A 6, [], [])]
ex9 = [(A 10, [], []), (A 4, [A 5], []), (A 4, [A 19], [A 11]), (A 12, [], []), (A 1, [], [A 2, A 3]), (A 2, [], [A 4]), (A 6, [], [])]
-- no T
ex10 = [(A 10,[],[A 19]),(A 4,[A 5],[]),(A 12,[],[A 22]),(A 1,[],[A 2,A 3]),(A 2,[],[A 4]),(A 6,[],[A 18])]
-- no N
ex11 = [(A 10,[],[A 4]),(A 4,[A 12],[]),(A 12,[],[]),(A 2,[],[A 2,A 4]),(A 6,[],[A 10])]

--loop
--ex3 = [(A 1, [A 3], []), (A 2, [A 1], [A 4]), (A 3, [], [A 2])]
