module MyTerms_test where

import qualified Data.Set as Set

import MyTerms



term1 =
  Term_Comp [
    Term_Atom "ahoj",
    Term_Comp [
      Term_Atom "ako",
      Term_Atom "sa",
      Term_Var "X" 0
    ],
    Term_Atom "haha",
    Term_Var "X" 0,
    Term_Var "X" 2
  ]

term2 =
  Term_Comp [
    Term_Atom "jaja",
    Term_Atom "-->",
    Term_Comp [
      Term_Atom "aha",
      Term_Atom "buj"
    ],
    Term_Var "X" 2
  ]

term3 =
  Term_Comp [
    Term_Atom "jaja",
    Term_Var "Z" 0,
    Term_Var "Y" 0,
    Term_Atom "bullshit_X"
  ]

term4 =
  Term_Comp [
    Term_Atom "jaja",
    Term_Comp [
      Term_Atom "mumumu",
      Term_Var "Z" 0
    ],
    Term_Atom "ahahaha",
    Term_Atom "bullshit_X"
  ]

term5 =
  Term_Comp [
    Term_Var "X" 0,
    Term_Var "X" 0
  ]

term6 =
  Term_Comp [
    Term_Atom "ahoj",
    Term_Atom "ahoj"
  ]

term7 =
  Term_Comp [
    Term_Var "X" 0,
    Term_Var "X" 2
  ]

term8 =
  Term_Lambda (Set.fromList [Var "X" 0, Var "Y" 2]) $
    Term_Comp [
      Term_Var "X" 0,
      Term_Var "Y" 2,
      Term_Lambda (Set.fromList [Var "Y" 2])$
        Term_Comp [
          Term_Atom "ahoj",
          Term_Var "Y" 2,
          Term_Var "Z" 0
        ]
    ]

term9 =
  Term_Lambda (Set.fromList [Var "X" 0, Var "Y" 2]) $
    Term_Comp [
      Term_Var "X" 0,
      Term_Var "Y" 2,
      Term_Lambda (Set.fromList [Var "Z" 4]) $
        Term_Comp [
          Term_Atom "ahoj",
          Term_Var "Z" 4,
          Term_Var "Z" 0
        ]
    ]

term10 =
  Term_Lambda (Set.fromList [Var "X" 0, Var "Y" 2]) $
    Term_Comp [
      Term_Atom "haha",
      Term_Var "Y" 2,
      Term_Lambda (Set.fromList [Var "Z" 4]) $
        Term_Comp [
          Term_Atom "ahoj",
          Term_Var "Z" 4,
          Term_Var "Z" 0
        ]
    ]

term11 =
  Term_Lambda (Set.fromList [Var "X" 0, Var "Y" 2]) $
    Term_Comp [
      Term_Var "X" 0,
      Term_Var "Y" 2,
      Term_Lambda (Set.fromList [Var "Z" 4]) $
        Term_Comp [
          Term_Atom "ahoj",
          Term_Var "Z" 4,
          Term_Atom "koniec"
        ]
    ]

term12 =
  Term_Lambda (Set.fromList [Var "X" 0, Var "Y" 2, Var "Z" 0]) $
    Term_Comp [
      Term_Var "X" 0,
      Term_Var "Y" 2,
      Term_Lambda (Set.fromList [Var "Y" 2])$
        Term_Comp [
          Term_Atom "ahoj",
          Term_Var "Y" 2,
          Term_Var "Z" 0
        ]
    ]
