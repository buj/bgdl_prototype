module Main where

import Data.Set.Monad as Mset

import qualified MyUtil as Mutil
import MyTerms
import MyParsing
import MyEngine
import MyBgRules



main :: IO ()
main = do
  text <- getContents
  let (ts, rest) = parseTerms text
  if not (rest == "")
    then putStrLn $ "Couldn't parse input, this remains:\n" ++ rest
    else
      let es = (esInitRules bgRules) `esAddTerms` ts
          models = fxEsEdge $ esFix es
      in putStrLn $ Mutil.lsWithSep "\n" (Mset.elems models)
