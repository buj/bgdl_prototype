module MyBgRules where

import MyTerms
import MyEngine



termLolli = termConn "--o"
termIsLolli = termIsConn "--o"

termPresent = termPrefix "present"
termIsPresent = termHasPrefix "present"

termConsumed = termPrefix "consumed"
termIsConsumed = termHasPrefix "consumed"

termApplied = termPrefix "applied"
termIsApplied = termHasPrefix "applied"

bgContraRule =
  chainRule [
    termConsumed $ tvarnum 0,
    termPresent $ tvarnum 0,
    termContra
  ]

bgImpRule =
  let x = tvarnum 0
      y = tvarnum 1
      impl = termImp x y
  in chainRule [
    impl, termNot $ termConsumed impl,
    x, termNot $ termConsumed x,
    packAnd [y, termPresent impl, termPresent x]
  ]

bgLolliRule =
  let x = tvarnum 0
      y = tvarnum 1
      lolli = termLolli x y
  in chainRule [
    lolli, termNot $ termConsumed lolli,
    x, termNot $ termConsumed x,
    packAnd [y, termPresent lolli, termConsumed x]
  ]

bgRules =
  [
    bgContraRule,
    bgImpRule,
    bgLolliRule
  ]
