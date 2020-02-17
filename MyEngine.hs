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

data Production = Produce_Term Term | Produce_Rule Rule | Retract deriving (Eq, Ord)

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


data Rule = Rule_Auto Auto | Rule_Trig Trigger

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

instance Eq Rule where
  (==) r1 r2 = ruleId r1 == ruleId r2

instance Ord Rule where
  (<=) r1 r2 = ruleId r1 <= ruleId r2


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

basicRules =
  [
    goalIntro,
    impElim,
    andIntro,
    orIntro1,
    orIntro2,
    contraIntro
  ]



-- engine: basics

type KeyMap = Map.Map TermKey [Term]
type Watchers = Map.Map TermKey (Set.Set Trigger)
type Succs = Map.Map Term [Production]

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

kmapAdd :: KeyMap -> Term -> KeyMap
kmapAdd kmap t =
  foldl (\m key -> Mutil.adjustOrInsert (t:) [t] key m) kmap $ termKeySubs t

kmapGet :: KeyMap -> TermKey -> [Term]
kmapGet kmap k = Map.findWithDefault [] k kmap

watsAddTrig :: Watchers -> Trigger -> Watchers
watsAddTrig wats trig = Mutil.adjustOrInsert (Set.insert trig) (Set.singleton trig) (trigKey trig) wats

watsAdd :: Watchers -> Rule -> Watchers
watsAdd wats (Rule_Trig trig) = watsAddTrig wats trig
watsAdd wats _ = wats

watsRemove :: Watchers -> Rule -> Watchers
watsRemove wats (Rule_Trig trig) = Map.adjust (Set.delete trig) (trigKey trig) wats
watsRemove wats _ = wats

watsGet :: Watchers -> TermKey -> Set.Set Trigger
watsGet wats k = Map.findWithDefault Set.empty k wats

succsAdd :: Succs -> Term -> Production -> Succs
succsAdd succs rname p = Mutil.adjustOrInsert (p:) [p] rname succs

succsGet :: Succs -> Term -> [Production]
succsGet succs rname = Map.findWithDefault [] rname succs



-- engine behaviour

esRetract :: EngineState -> Production -> EngineState
esRetract es@(EState _ pending _ _ succs) p =
  case p of
    Produce_Term t  -> es { esPending = Set.delete t pending }
    Produce_Rule r  ->
      let rname = ruleId r
          prods = succs `succsGet` rname
          es'@(EState _ _ _ wats' succs') = foldl esRetract es prods
      in es' {
        esWatchers = wats' `watsRemove` r,
        esSuccs = Map.delete rname succs'
      }


esTriggerInstance :: EngineState -> Trigger -> Term -> EngineState
esTriggerTrig :: EngineState -> Trigger -> EngineState
esTriggerTerm :: EngineState -> Term -> EngineState

esAddProd :: EngineState -> Rule -> Production -> EngineState
esAddRule :: EngineState -> Rule -> EngineState
esAddTerm :: EngineState -> Term -> EngineState

esAddProds :: EngineState -> Rule -> [Production] -> EngineState
esAddProds es r prods = foldl (flip esAddProd $ r) es prods

esAuto :: EngineState -> Rule -> EngineState
esAuto es r = esAddProds es r $ ruleAutos r

esTriggerInstance es trig@(Trigger _ _ see _) t = esAddProds es (Rule_Trig trig) $ see t
esTriggerTrig es@(EState _ _ kmap _ _) trig =
  foldl (flip esTriggerInstance $ trig) es (kmap `kmapGet` (trigKey trig))
esTriggerTerm es@(EState _ _ _ wats _) t =
  foldl (\es trig -> esTriggerInstance es trig t) es (wats `watsGet` (termKey t))

esAddProd es@(EState _ pending _ _ succs) r p =
  let rname = ruleId r
      es' = es { esSuccs = succsAdd succs rname p }
  in case p of
    Produce_Term tgt  -> es' { esPending = Set.insert tgt pending }
    Produce_Rule tgt  -> esAddRule es' tgt
    Retract           -> esRetract es (Produce_Rule r)

esAddRule es r =
  let es1@(EState _ _ _ wats1 _) = esAuto es r
      es2 = es1 { esWatchers = wats1 `watsAdd` r }
  in case r of
    Rule_Trig trig  -> es2 `esTriggerTrig` trig
    _               -> es2

esAddTerm es@(EState ts _ kmap _ _) t =
  esTriggerTerm (es { esTerms = Set.insert t ts, esKeys = kmap `kmapAdd` t }) t


esConsumeTerm :: EngineState -> Term -> EngineState
esConsumeTerm es@(EState _ pending _ _ _) t
  | (t `Set.member` pending) = esAddTerm (es { esPending = Set.delete t pending }) t
  | otherwise = es

esStep :: EngineState -> Mset.Set EngineState
esStep es@(EState _ pending _ _ _) = Mset.fromList $ map (esConsumeTerm es) (Set.elems pending)

fixMset :: Ord a => (a -> Mset.Set a) -> (Mset.Set a, Mset.Set a) -> Mset.Set a
fixMset f (past, curr)
  | not (Mset.size curr == 0) =
      let npast = past `Mset.union` curr
          ncurr = (curr >>= f) `Mset.difference` npast
      in fixMset f (npast, ncurr)
  | otherwise = past

esFix :: EngineState -> Mset.Set EngineState
esFix es = fixMset esStep (Mset.empty, Mset.singleton es)


-- engine: convenience functions

esAddRules :: EngineState -> [Rule] -> EngineState
esAddRules = foldl esAddRule

esInitRules :: [Rule] -> EngineState
esInitRules = esAddRules esEmpty

esAddTerms :: EngineState -> [Term] -> EngineState
esAddTerms = foldl esAddTerm
