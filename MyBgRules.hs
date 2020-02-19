module MyBgRules where

import MyTerms
import MyEngine



termProduced = termPrefix "+"
termIsProduced = termHasPrefix "+"

termLolli = termConn "--o"
termIsLolli = termIsConn "--o"

termConsumed = termPrefix "-"
termIsConsumed = termHasPrefix "-"

bgProduceRule =
  chainRule [
    termProduced $ tvarnum 0,
    termNot $ (termConsumed $ tvarnum 0),
    tvarnum 0
  ]

bgContraRule =
  chainRule [
    termConsumed $ tvarnum 0,
    tvarnum 0,
    termContra
  ]

bgImpRule =
  let x = tvarnum 0
      y = tvarnum 1
      impl = termImp x y
  in chainRule [
    impl, -- not $ termConsumed impl,
    x, -- not $ termConsumed x,
    termProduced y
  ]

bgLolliRule =
  let x = tvarnum 0
      y = tvarnum 1
      lolli = termLolli x y
  in chainRule [
    lolli, -- termNot $ termConsumed lolli,
    termProduced x, termNot $ termConsumed x,
    packAnd [termProduced y, termConsumed x]
  ]

bgRules =
  [
    bgProduceRule,
    bgContraRule,
    bgImpRule,
    bgLolliRule
  ]
