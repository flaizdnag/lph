module ExamplesToTest
    ( lp1
    , lp2
    , lp3
    ) where

import LogicPrograms


lp1 :: LP
lp1 =
    [ Fact (A 1 ""), Assumption (A 1 "")
    , Fact (A 2 ""), Assumption (A 2 "")
    ]

lp2 :: LP
lp2 =
    [ Cl (A 1 "") [A 10 "", A 10 ""] [A 11 "", A 11 ""]
    , Cl (A 2 "") [A 15 "", A 15 ""] [A 16 "", A 16 ""]
    , Fact (A 1 ""), Assumption (A 1 "")
    , Fact (A 2 ""), Assumption (A 2 "")
    ]

lp3 :: LP
lp3 = 
    [ Cl (A 1 "") [A 2 "", A 3 ""] [A 4 "", A 5 ""]
    , Cl (A 2 "") [A 1 "", A 3 ""] [A 4 "", A 5 ""]
    , Fact (A 3 "")
    , Assumption (A 4 "")
    ]

{-
Logic program (LP):
[(A 1, [A 2, A 3], [A 4])
,(A 5, [A 6], [])
,(A 10, [], [])
,(A 1, [], [])]

compP LP:
[E (V (A 1)) (D [C [V (A 2),V (A 3),N (V (A 4))],T])
,E (V (A 5)) (V (A 6))
,E (V (A 10)) T
,N (V (A 2))
,N (V (A 3))
,N (V (A 4))
,N (V (A 6))]

groupByValue (compP LP): [[Un], [T], [F]]
[
    [E (V (A 1)) (D [C [V (A 2),V (A 3),N (V (A 4))],T]), E (V (A 5)) (V (A 6))],
    [E (V (A 10)) T],
    [N (V (A 2)), N (V (A 3)), N (V (A 4)), N (V (A 6))]
]

interp LP: ([T], [F])
(
    [V (A 10)],
    [V (A 2),V (A 3),V (A 4),V (A 6)]
)

perms [(A 1, [A 2], [A 4])] ([], []):
[
    [],
    [V (A 1)],
    [V (A 2)],
    [N (V (A 4))],
    [V (A 1),V (A 2)],
    [V (A 1),N (V (A 4))],
    [V (A 2),N (V (A 4))],
    [V (A 1),V (A 2),N (V (A 4))]
]

perms [(A 1, [A 2], [A 4])] ([V (A 1), V (A 2)], []):
[
    [],
    [N (V (A 4))]
]
-}
