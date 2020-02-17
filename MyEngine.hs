{-# LANGUAGE TupleSections #-}

module MyEngine where

import Data.Maybe
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Set.Monad as Mset

import MyTerms
import qualified MyUtil as Mutil



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

termSub = termConnective "sub"
termImp = termConnective "==>"
termAnd = termConnective "and"
termOr = termConnective "or"



-- rules

data Production = Produce_Term Term | Produce_Rule Rule | Retract

data Auto = Auto { autoId :: Term, autoProds :: [Production] }
data Trigger =  Trigger {
  trigId :: Term, trigEye :: TermKey,
  trigSee :: Term -> [Production], trigProds :: [Production]
}

instance Eq Auto where
  (==) (Auto name1 _) (Auto name2 _) = name1 == name2

instance Ord Auto where
  (<=) (Auto name1 _) (Auto name2 _) = name1 <= name2

instance Eq Trigger where
  (==) (Trigger name1 _ _ _) (Trigger name2 _ _ _) = name1 == name2

instance Ord Trigger where
  (<=) (Trigger name1 _ _ _) (Trigger name2 _ _ _) = name1 <= name2

data Rule = Rule_Auto Auto | Rule_Trig Trigger deriving (Eq, Ord)

autoRule :: Term -> [Production] -> Rule
autoRule t ps = Rule_Auto $ Auto t ps

trigRule :: Term -> TermKey -> (Term -> [Production]) -> [Production] -> Rule
trigRule t key see prods = Rule_Trig $ Trigger t key see prods

ruleId :: Rule -> Term
ruleId (Rule_Auto (Auto name _)) = name
ruleId (Rule_Trig (Trigger name _ _ _)) = name

autoKey = termKey . autoId
trigKey = termKey . trigId
ruleKey = termKey . ruleId

ruleAutos :: Rule -> [Production]
ruleAutos (Rule_Auto (Auto _ prods)) = prods
ruleAutos (Rule_Trig (Trigger _ _ _ prods)) = prods


termChain :: [Term] -> Term
termChain ts = Term_Comp ((Term_Atom "#chain"):ts)

chainRule :: [Term] -> Rule
chainRule ts@(t:[]) = autoRule (termChain ts) [Produce_Term t]
chainRule ts@(t:tail)
  | (termIsNot t) =
      let tcore = fromJust $ termFromNot t
      in  trigRule rname (termKey tcore) (
            \cand -> maybeToList $ patternMatch tcore cand >> Just Retract
          ) [Produce_Rule (chainRule tail)]
  | otherwise =
      trigRule rname (termKey t) (
        \cand -> maybeToList $ do
          mapping <- patternMatch t cand
          return $ Produce_Rule (chainRule (map (substituteMany mapping) tail))
      ) []
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



-- engine

type KeyMap = Map.Map TermKey [Term]
type Watchers = Map.Map TermKey (Set.Set Trigger)
type Succs = Map.Map Rule [Production]

data EngineState =
  EState {
    esTerms ::    Set.Set Term,
    esPending ::  Set.Set Term,
    esKeys ::     KeyMap,
    esWatchers :: Watchers,
    esSuccs ::    Succs
  }

instance Eq EngineState where
  (==) es1 es2 = (esTerms es1 == esTerms es2)

instance Ord EngineState where
  (<=) es1 es2 = (esTerms es1 <= esTerms es2)

esEmpty :: EngineState
esEmpty = EState Set.empty Set.empty Map.empty Map.empty Map.empty

esIsFixed :: EngineState -> Bool
esIsFixed es = Set.size (esPending es) == 0

esK_add :: KeyMap -> Term -> KeyMap
esK_add kmap t =
  foldl (\m key -> Mutil.adjustOrInsert (t:) [t] key m) kmap $ termKeySubs t

esW_addTrig :: Watchers -> Trigger -> Watchers
esW_addTrig wats trig = Mutil.adjustOrInsert (Set.insert trig) (Set.singleton trig) (trigKey trig) wats

esW_add :: Watchers -> Rule -> Watchers
esW_add wats (Rule_Trig trig) = esW_addTrig wats trig
esW_add wats _ = wats

esW_remove :: Watchers -> Rule -> Watchers
esW_remove wats (Rule_Trig trig) = Map.adjust (Set.delete trig) (trigKey trig) wats
esW_remove wats _ = wats

esS_add :: Succs -> Rule -> Production -> Succs
esS_add succs r p = Mutil.adjustOrInsert (p:) [p] r succs


esRetract :: EngineState -> Production -> EngineState
esRetract es@(EState _ pending _ _ succs) p =
  case p of
    Produce_Term t  -> es { esPending = Set.delete t pending }
    Produce_Rule r  ->
      let prods = Map.findWithDefault [] r succs
          es'@(EState _ _ _ wats' succs') = foldl esRetract es prods
      in es' {
        esWatchers = esW_remove wats' r,
        esSuccs = Map.delete r succs'
      }

{-
esTriggerInstance :: EngineState -> Rule -> Term -> EngineState
esTriggerRule :: EngineState -> Rule -> EngineState
esTriggerTerm :: EngineState -> Term -> EngineState

esAddProd :: EngineState -> Rule -> Production -> EngineState
esAddRule :: EngineState -> Rule -> EngineState
esAddTerm :: EngineState -> Term -> EngineState

esRuleAuto :: EngineState -> Rule -> EngineState
esRuleAuto es r@(Rule _ _ autos) = foldl (flip esAddProd $ r) es autos

esTriggerInstance es r@(Rule name (Just (Trigger _ see)) _) t =

esAddProd es@(EState _ pending _ _ esS) r p =
  let es' = es { esSuccs = esS_add esS r p }
  in case p of
    Produce_Term tgt  -> es' { esPending = Set.insert tgt pending }
    Produce_Rule tgt  -> esAddRule es' tgt
    Retract           -> esRetract es (Produce_Rule r)


esConsumeTerm :: EngineState -> Term -> EngineState
esConsumeTerm es@(EState _ pending _ _ _) t
  | (t `Set.member` pending) = esAddTerm (es { esPending = Set.delete t pending }) t
  | otherwise = es

esStep :: EngineState -> Mset.Set EngineState
esStep es@(EState _ pending _ _ _) = Set.map (esConsumeTerm es) pending

fixMset :: (a -> Mset.Set a) -> (Mset.Set a, Mset.Set a) -> Mset.Set a
fixMset f (past, curr)
  | not (Set.size curr == 0) =
      let npast = past `Set.union` curr
          ncurr = (curr >>= f) `Set.difference` npast
      in fixSet f (npast, ncurr)
  | otherwise = past

esFix :: EngineState -> Mset.Set EngineState
esFix es = fixSet esStep (Set.empty, Set.singleton es)
-}
