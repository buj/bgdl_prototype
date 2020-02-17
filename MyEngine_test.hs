module MyEngine_test where

import MyTerms
import MyEngine
import MyUtil



basicRules = [impElim]

es0 = esInitRules basicRules

kb0 =
  [
    tatom "a",
    termImp (tatom "a") (tatom "b")
  ]

kb1 =
  [
    tatom "a",
    tatom "b",
    termImp (termAnd (tatom "a") (tatom "b")) (tatom "c")
  ]

kb2 =
  [
    termImp (termNot $ tatom "a") (tatom "b"),
    termImp (termNot $ tatom "b") (tatom "a"),
    termImp (termNot $ tatom "c") (tatom "c")
  ]
