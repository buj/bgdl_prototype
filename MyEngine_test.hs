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

kb3 =
  [
    termImp (termNot $ tatom "a") (tatom "b"),
    termImp (termNot $ tatom "b") (tatom "c"),
    termImp (termNot $ tatom "c") (tatom "a")
  ]

kb4 =
  [
    termImp (termNot $ tatom "a") (tatom "b"),
    termImp (termNot $ tatom "b") (tatom "c"),
    termImp (termNot $ tatom "c") (tatom "d"),
    termImp (termNot $ tatom "d") (tatom "a")
  ]

kb5 =
  [
    tatom "a",
    tatom "b",
    termImp (termAnd (tatom "a") (tatom "b")) (termAnd (tatom "c") (tatom "d"))
  ]

kb6 =
  [
    tatom "a",
    tatom "b",
    termImp (termAnd (tatom "a") (tatom "b")) (tatom "c"),
    termImp (tatom "c") termContra
  ]
