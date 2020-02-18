module MyParsing where

import Data.Char
import Data.Maybe

import MyTerms



readWhite :: String -> String
readWhite str@(ch:rest)
  | (isSpace ch) = readWhite rest
  | otherwise = str
readWhite "" = ""

readChar :: Char -> String -> Maybe String
readChar ch1 (ch2:rest)
  | (ch1 == ch2) = Just rest
  | otherwise = Nothing
readChar _ _ = Nothing

isGoodSymbol :: Char -> Bool
isGoodSymbol ch = (isLetter ch) || (isNumber ch) || (ch `elem` ['-', '=', '>'])

_parseName :: String -> String -> (String, String)
_parseName acc str@(ch:rest)
  | (isGoodSymbol ch) = _parseName (ch:acc) rest
  | otherwise = (reverse acc, str)
_parseName acc "" = (reverse acc, "")

parseName :: String -> Maybe (String, String)
parseName str =
  let (name, rest) = _parseName [] str
  in  if name == "" then Nothing
      else Just (name, rest)

parseVar :: String -> Maybe (Term, String)
parseVar ('?':rest) = do
  (name, rest1) <- parseName rest
  return (Term_Var name 0, rest1)
parseVar _ = Nothing

parseAtom :: String -> Maybe (Term, String)
parseAtom str = do
  (name, rest) <- parseName str
  return (Term_Atom name, rest)

parseTerm :: String -> Maybe (Term, String)

_parseTerms :: [Term] -> String -> ([Term], String)
_parseTerms acc str =
  let mbyTerm = parseTerm str
  in  if isJust mbyTerm
      then  let (t1, rest) = fromJust mbyTerm
            in  _parseTerms (t1:acc) $ readWhite rest
      else  (reverse acc, str)

parseTerms :: String -> ([Term], String)
parseTerms = _parseTerms []

parseComp :: String -> Maybe (Term, String)
parseComp ('(':rest) =
  let (ts, rest1) = _parseTerms [] $ readWhite rest
  in  if  ts == []  then Nothing
      else do
        rest2 <- readChar ')' $ readWhite rest1
        return (Term_Comp ts, rest2)
parseComp _ = Nothing

parseTerm str
  | (isJust v) = v
  | (isJust a) = a
  | (isJust c) = c
  | otherwise = Nothing
  where v = parseVar str
        a = parseAtom str
        c = parseComp str
