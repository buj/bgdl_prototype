module MyBgRules_test where

import MyTerms
import MyEngine
import MyBgRules



es0 = esInitRules bgRules

kb0 =
  [
    tatom "a",
    tatom "b"
  ]

kb1 =
  [
    tatom "a",
    termImp (tatom "a") (tatom "b")
  ]

kb2 =
  [
    tatom "a",
    termLolli (tatom "a") (tatom "c")
  ]

kb3 =
  [
    tatom "a",
    termImp (tatom "a") (tatom "b"),
    termLolli (tatom "a") (tatom "c")
  ]

kb4 =
  [
    tatom "a",
    termLolli (tatom "a") (tatom "b"),
    termLolli (tatom "a") (tatom "c")
  ]
