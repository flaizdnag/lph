module ExamplesToTest
    ( lp1
    , lp2
    , lp3
    , lp4
    , lp5
    , forms1
    ) where

import LogicPrograms
import CPL


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

lp4 :: LP
lp4 = [Cl (A 1 "") [A 2 ""] [A 1 ""]]

lp5 :: LP
lp5 =
    [ Cl (A 2 "") [A 1 ""] []
    , Cl (A 1 "") [] [A 2 ""]
    , Cl (A 2 "") [] [A 1 ""]
    ]


forms1 :: [Form]
forms1 =
    [ V (A 1 "")
    , N (V (A 2 ""))
    , C [V (A 3 ""), N (V (A 4 ""))]
    , D [C [V (A 5 ""), N (V (A 6 ""))], C [V (A 7 ""), N (V (A 8 ""))]]
    , E (V (A 9 "")) (N (V (A 10 "")))
    , E (V (A 11 "")) (D [C [V (A 12 "")]])
    ]
