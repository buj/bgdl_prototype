module MyEngine where

import Data.Maybe
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

import MyTerms



-- terms with special meaning: manipulation / creation / ...

termContra = tatom "#bot"

termPrefix :: String -> Term -> Term
termPrefix pref t = Term_Comp [Term_Atom ('#':pref), t]

termDrop :: String -> Term -> Maybe Term
termDrop pref (Term_Comp ((Term_Atom ('#':cand)):t:[]))
  | (pref == cand) = Just t
  | otherwise = Nothing
termDrop _ _ = Nothing

termHasPref :: String -> Term -> Bool
termHasPref pref = isNothing . (termDrop pref)

termGoal = termPrefix "goal"
termFromGoal = termDrop "goal"
termIsGoal = termHasPref "goal"

termNot = termPrefix "not"
termFromNot = termDrop "not"
termIsNot = termHasPref "not"

termForall :: Term -> Term
termForall t = Term_Comp [Term_Atom "#forall", termClosure t]

termFromForall :: Term -> Maybe Term
termFromForall (Term_Comp ((Term_Atom "#forall"):t:[])) =
  case t of
    Term_Lambda _ sub   -> Just sub
    _                   -> Just t
termFromForall _ = Nothing

termIsForall = termHasPref "forall"


termConnective :: String -> Term -> Term -> Term
termConnective conn t1 t2 = Term_Comp [t1, Term_Atom ('#':conn), t2]

sidesFrom :: String -> Term -> Maybe (Term, Term)
sidesFrom conn (Term_Comp (lhs:(Term_Atom mid):rhs:[]))
  | (mid == conn) = Just (lhs, rhs)
  | otherwise = Nothing

lhsFrom :: String -> Term -> Maybe Term
lhsFrom conn t = fst <$> sidesFrom conn t

rhsFrom :: String -> Term -> Maybe Term
rhsFrom conn t = snd <$> sidesFrom conn t

subsFrom :: String -> Term -> Maybe [Term]
subsFrom conn (Term_Comp (t1:(Term_Atom mid):t2:[]))
  | (mid == conn) = Just [t1, t2]
  | otherwise = Nothing

termImp = termConnective "==>"
termAnd = termConnective "and"
termOr = termConnective "or"



-- rules

data Production = Produce_Term Term | Produce_Rule Rule | Retract

data RuleEffect =
  Reff_Trigger { reffEye :: TermKey, reffSee :: Term -> [Production] }  |
  Reff_Auto { reffAuto :: [Production] }

data Rule = Rule {
  ruleId :: Term,
  rulePred :: Maybe Term,
  ruleEff :: [RuleEffect]
}

instance Eq Rule where
  (==) r1 r2 = ruleId r1 == ruleId r2

instance Ord Rule where
  (<=) r1 r2 = ruleId r1 <= ruleId r2

termChain :: [Term] -> Term
termChain ts = Term_Comp ((Term_Atom "#chain"):ts)

chainRule :: [Term] -> Maybe Term -> Rule
chainRule ts@(t:[]) pred = Rule (termChain ts) pred [Reff_Auto [Produce_Term t]]
chainRule ts@(t:tail) pred
  | (termIsNot t) =
      let tcore = fromJust $ termFromNot t
      in Rule rname pred [
        Reff_Auto [Produce_Rule (chainRule tail $ Just rname)],
        Reff_Trigger (termSgn tcore) $
          \cand -> maybeToList $ patternMatch tcore cand >> Just Retract
      ]
  | otherwise =
      Rule rname pred [
        Reff_Trigger (termSgn t) $
          \cand -> maybeToList $ do
            mapping <- patternMatch t cand
            return $ Produce_Rule (chainRule (map (substituteMany mapping) tail) $ Just rname)
      ]
  where rname = termChain ts

goalIntro =
  chainRule [termImp (tvarnum 0) (tvarnum 1), termGoal (tvarnum 0)]

impElim =
  chainRule [termImp (tvarnum 0) (tvarnum 1), tvarnum 0, tvarnum 1]

andIntro = 
  chainRule [termGoal conj, tvarnum 0, tvarnum 1, conj]
  where conj = termAnd (tvarnum 0) (tvarnum 1)

orIntro1 =
  chainRule [termGoal disj, tvarnum 0, disj]
  where disj = termOr (tvarnum 0) (tvarnum 1)

orIntro2 =
  chainRule [termGoal disj, tvarnum 1, disj]
  where disj = termOr (tvarnum 0) (tvarnum 1)

contraIntro =
  chainRule [termNot (tvarnum 0), tvarnum 0, termContra]
