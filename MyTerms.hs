module MyTerms where

import Control.Monad
import Control.Monad.Fix
import Data.List.Index
import Data.Maybe
import Data.Tuple
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Sequence as Seq

import MyUtil


-- basic definitions

data Atom = Atom { atomId :: String } deriving (Eq, Ord)
data Variable = Var { varName :: String, varIndex :: Int } deriving (Eq, Ord)
data Term = Term_Var String Int | Term_Atom String | Term_Comp [Term] deriving Eq


varToTerm :: Variable -> Term
varToTerm (Var name i) = Term_Var name i

atomToTerm :: Atom -> Term
atomToTerm = Term_Atom . atomId

termToVar :: Term -> Maybe Variable
termToVar (Term_Var name i) = Just $ Var name i
termToVar _ = Nothing

termToAtom :: Term -> Maybe Atom
termToAtom (Term_Atom name) = Just $ Atom name
termToAtom _ = Nothing

termIsVar :: Term -> Bool
termIsVar (Term_Var _ _) = True
termIsVar _ = False

termIsAtom :: Term -> Bool
termIsAtom (Term_Atom _) = True
termIsAtom _ = False

termIsComp :: Term -> Bool
termIsComp (Term_Comp _) = True
termIsComp _ = False


instance Show Variable where
  show (Var name i) = "?" ++ name ++ "_" ++ show i

instance Show Atom where
  show = atomId

instance Show Term where
  show t@(Term_Var _ _) = show (fromJust $ termToVar t)
  show t@(Term_Atom _) = show (fromJust $ termToAtom t)
  show (Term_Comp subs) = "(" ++ (unwords $ map show subs) ++ ")"



-- term structure manipulation/inspection

substitute :: Variable -> Term -> Term -> Term
substitute v t tgt =
  case tgt of
    Term_Var _ _    -> if (Just v == termToVar t) then t else tgt
    Term_Atom _     -> tgt
    Term_Comp subs  -> Term_Comp $ map (substitute v t) subs

termSub :: Term -> Variable -> Term -> Term
termSub tgt v t = substitute v t tgt

substituteMany :: Map.Map Variable Term -> Term -> Term
substituteMany mapping tgt =
  case tgt of
    Term_Var name i -> Map.findWithDefault tgt (Var name i) mapping
    Term_Atom _     -> tgt
    Term_Comp subs  -> Term_Comp $ map (substituteMany mapping) subs

termSubMany :: Term -> Map.Map Variable Term -> Term
termSubMany = flip substituteMany


type VarCollection = Map.Map String (Set.Set Int)

addVarsFrom :: Term -> VarCollection -> VarCollection
addVarsFrom (Term_Var name i) =
  (Map.adjust (Set.insert i) name) . (Map.insertWith (flip const) name Set.empty)
addVarsFrom (Term_Atom _) = id
addVarsFrom (Term_Comp subs) =
  \vc -> foldl vcCollect vc subs

vcCollect :: VarCollection -> Term -> VarCollection
vcCollect = flip addVarsFrom

termGetVc :: Term -> VarCollection
termGetVc t = addVarsFrom t Map.empty


vcVarMaxIndices :: VarCollection -> Map.Map String Int
vcVarMaxIndices = Map.map (fromJust . Set.lookupMax)

termVarMaxIndices :: Term -> Map.Map String Int
termVarMaxIndices = vcVarMaxIndices . termGetVc


setIntGetCompressLs :: Set.Set Int -> [(Int, Int)]
setIntGetCompressLs s = map swap (indexed $ Set.elems s)

vcGetCompressMap :: VarCollection -> Map.Map Variable Variable
vcGetCompressMap vc =
  let ls1 = Map.assocs $ Map.map setIntGetCompressLs vc
      ls2 = ls1 >>= (\(name, iis) -> map (\(i, j) -> (Var name i, Var name j)) iis)
  in Map.fromAscList ls2

termGetCompressMap :: Term -> Map.Map Variable Variable
termGetCompressMap = vcGetCompressMap . termGetVc

termCompressVars :: Term -> Term
termCompressVars t =
  let vc = termGetVc t
      mapping = Map.map varToTerm $ vcGetCompressMap vc
  in substituteMany mapping t


separateTerms :: Term -> Term -> (Term, Term)
separateTerms t1 t2 =
  let t1' = termCompressVars t1
      t1'vmi = termVarMaxIndices t1
      t2map = termGetCompressMap t2
      t2map' = Map.map (\(Var name i) -> Term_Var name $ i + (Map.findWithDefault 0 name t1'vmi)) t2map
      t2' = substituteMany t2map' t2
  in (t1', t2')


-- term pattern matching (0) / unification (1)

containsVar :: Variable -> Term -> Bool
containsVar v t =
  case t of
    Term_Var _ _    -> (Just v == termToVar t)
    Term_Comp subs  -> or $ map (containsVar v) subs
    _               -> False

occursCheck :: Variable -> Term -> Bool
occursCheck v t
  | (Just v == termToVar t) = False
  | otherwise = containsVar v t

data EqState = EqState {
  eqs :: Seq.Seq (Term, Term),
  mapping :: Map.Map Variable Term,
  failed :: Bool
}

eqsInit :: [(Term, Term)] -> EqState
eqsInit eqs = EqState (Seq.fromList eqs) Map.empty False

eqsFinished :: EqState -> Bool
eqsFinished (EqState eqs0 _ failed0) = (Seq.length eqs0 == 0 || failed0)

unAddMapping :: Variable -> Term -> EqState -> EqState
unAddMapping v t state@(EqState eqs0 mapping0 _)
  | (Just v == termToVar t) = state
  | otherwise =
    let eqs1 = fmap (mapPair $ substitute v t) eqs0
        mapping1 = Map.insert v t $ Map.map (substitute v t) mapping0
    in state { eqs = eqs1, mapping = mapping1 }

pmAddMapping :: Variable -> Term -> EqState -> EqState
pmAddMapping v t state@(EqState _ mapping0 _)
  | let currMapping = Map.lookup v mapping0,
    not (currMapping == Nothing) =
      if currMapping == Just t then state
      else state { failed = True }
  | otherwise = state { mapping = Map.insert v t mapping0 }

type EqStep = Term -> Term -> EqState -> EqState

compStep :: EqStep
compStep (Term_Atom lname) (Term_Atom rname) state = state { failed = not (lname == rname) }
compStep (Term_Comp lsubs) (Term_Comp rsubs) state@(EqState eqs0 _ _)
  | not (length lsubs == length rsubs) = state { failed = True }
  | otherwise = state { eqs = eqs0 Seq.>< (Seq.fromList $ zip lsubs rsubs) }
compStep _ _ state = state { failed = True }

unStep :: EqStep
unStep left right state
  | (termIsVar left) =
    let lvar = fromJust $ termToVar left
    in
    if (not $ occursCheck lvar right) then unAddMapping lvar right state
    else state { failed = True }
  | (termIsVar right) = unStep right left state
  | otherwise = compStep left right state

pmStep :: EqStep
pmStep left right state
  | (termIsVar left) = pmAddMapping (fromJust $ termToVar left) right state
  | (termIsVar right) = state { failed = True }
  | otherwise = compStep left right state

eqsRecurse :: EqStep -> EqState -> EqState
eqsRecurse step state@(EqState eqs0 _ _)
  | eqsFinished state = state
  | otherwise =
    let (left, right) = fromJust $ Seq.lookup 0 eqs0
        state1 = state { eqs = Seq.drop 1 eqs0 }
    in eqsRecurse step $ step left right state1

type EqMapping = Maybe (Map.Map Variable Term)

eqsToRes :: EqState -> EqMapping
eqsToRes state@(EqState _ mapping0 failed0)
  | failed0 = Nothing
  | otherwise = Just mapping0

eqsSolve :: EqStep -> [(Term, Term)] -> EqMapping
eqsSolve step eqs = eqsToRes $ eqsRecurse step (eqsInit eqs)

unifyMany :: [(Term, Term)] -> EqMapping
unifyMany = eqsSolve unStep

patternMatchMany :: [(Term, Term)] -> EqMapping
patternMatchMany = eqsSolve pmStep

unify :: Term -> Term -> EqMapping
unify left right = unifyMany [(left, right)]

patternMatch :: Term -> Term -> EqMapping
patternMatch left right = patternMatchMany [(left, right)]


-- test instances

term1 =
  Term_Comp [
    Term_Atom "ahoj",
    Term_Comp [
      Term_Atom "ako",
      Term_Atom "sa",
      Term_Var "X" 0
    ],
    Term_Atom "haha",
    Term_Var "X" 0,
    Term_Var "X" 2
  ]

term2 =
  Term_Comp [
    Term_Atom "jaja",
    Term_Atom "-->",
    Term_Comp [
      Term_Atom "aha",
      Term_Atom "buj"
    ],
    Term_Var "X" 2
  ]

term3 =
  Term_Comp [
    Term_Atom "jaja",
    Term_Var "Z" 0,
    Term_Var "Y" 0,
    Term_Atom "bullshit_X"
  ]

term4 =
  Term_Comp [
    Term_Atom "jaja",
    Term_Comp [
      Term_Atom "mumumu",
      Term_Var "Z" 0
    ],
    Term_Atom "ahahaha",
    Term_Atom "bullshit_X"
  ]

term5 =
  Term_Comp [
    Term_Var "X" 0,
    Term_Var "X" 0
  ]

term6 =
  Term_Comp [
    Term_Atom "ahoj",
    Term_Atom "ahoj"
  ]

term7 =
  Term_Comp [
    Term_Var "X" 0,
    Term_Var "X" 2
  ]
