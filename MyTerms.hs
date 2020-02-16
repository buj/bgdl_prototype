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
data Term =
  Term_Var String Int |
  Term_Atom String |
  Term_Comp [Term] |
  Term_Lambda (Set.Set Variable) Term
  deriving (Eq, Ord)


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

termIsLambda :: Term -> Bool
termIsLambda (Term_Lambda _ _) = True
termIsLambda _ = False

varNextIndex :: Variable -> Variable
varNextIndex (Var name i) = (Var name (i+1))


instance Show Variable where
  show (Var name i) = "?" ++ name ++ "_" ++ show i

instance Show Atom where
  show = atomId

instance Show Term where
  show t@(Term_Var _ _) = show (fromJust $ termToVar t)
  show t@(Term_Atom _) = show (fromJust $ termToAtom t)
  show (Term_Comp subs) = "(" ++ (unwords $ map show subs) ++ ")"
  show (Term_Lambda vars sub) = "\\" ++ (show $ Set.elems vars) ++ "." ++ show sub 



-- term structure manipulation/inspection

type VarSet = Set.Set Variable
type VarBank = Map.Map String Int

termMergeLambdas :: Term -> Term
termMergeLambdas (Term_Lambda vars1 (Term_Lambda vars2 sub)) = Term_Lambda (vars1 `Set.union` vars2) sub
termMergeLambdas (Term_Lambda vars sub) = Term_Lambda vars $ termMergeLambdas sub
termMergeLambdas (Term_Comp subs) = Term_Comp $ map termMergeLambdas subs
termMergeLambdas t = t

termFreeVars :: Term -> VarSet
termFreeVars (Term_Var name i) = Set.singleton $ Var name i
termFreeVars (Term_Atom _) = Set.empty
termFreeVars (Term_Comp subs) = foldl Set.union Set.empty $ map termFreeVars subs
termFreeVars (Term_Lambda vars sub) = termFreeVars sub `Set.difference` vars


vbGetNext :: VarBank -> String -> Variable
vbGetNext vb name = Var name $ Map.findWithDefault 0 name vb

vbMax :: VarBank -> Variable -> VarBank
vbMax vb (Var name i) = Map.insertWith max name i vb

vbRegister :: VarBank -> Variable -> VarBank
vbRegister vb = vbMax vb . varNextIndex

vbMerge :: VarBank -> VarBank -> VarBank
vbMerge vb1 vb2 = foldl vbMax Map.empty (map (uncurry Var) $ Map.assocs vb1 ++ Map.assocs vb2)

getFirstUnusedIndices :: VarSet -> VarBank
getFirstUnusedIndices vars = Set.foldl vbRegister Map.empty vars


type VarMap = Map.Map Variable Variable

data TminState = TminState { tminVb :: VarBank, tminMapping :: VarMap }

vbToTs :: VarBank -> TminState
vbToTs vb = TminState vb Map.empty

tsMinimizeIndices :: TminState -> Term -> Term
tsMinimizeIndices ts0@(TminState vb0 mapping0) t =
  case t of
    Term_Var name i       ->
      let v = Var name i
      in varToTerm $ Map.findWithDefault v v mapping0
    Term_Atom _           -> t
    Term_Comp subs        -> Term_Comp $ map (tsMinimizeIndices ts0) subs
    Term_Lambda vars sub  ->
      let unVars = vars `Set.difference` (Map.keysSet mapping0)
          ts1 = Set.foldl (
                  \ts@(TminState vb mapping) v@(Var name _) ->
                  let img = vb `vbGetNext` name
                  in TminState (vb `vbRegister` img) (Map.insert v img mapping)
                ) ts0 vars
          vars1 = Set.map ((Map.!) $ tminMapping ts1) vars
      in Term_Lambda vars1 $ tsMinimizeIndices ts1 sub

termMinimizeIndices :: Term -> Term
termMinimizeIndices t = tsMinimizeIndices (vbToTs . getFirstUnusedIndices . termFreeVars $ t) t

termNormalize :: Term -> Term
termNormalize = termMergeLambdas . termMinimizeIndices

termRepel :: [Term] -> Term -> Term
termRepel ts tgt = tsMinimizeIndices (vbToTs $ getFirstUnusedIndices (Set.unions $ map termFreeVars (tgt:ts))) tgt



-- alpha equivalence

data AlphaState = AState {
  asLbound :: VarSet, asRbound :: VarSet,
  asLmap :: VarMap, asRmap :: VarMap
}

asInit :: AlphaState
asInit = AState Set.empty Set.empty Map.empty Map.empty

asAddBounds :: AlphaState -> VarSet -> VarSet -> AlphaState
asAddBounds as@(AState lb0 rb0 _ _) lset rset =
  as {
    asLbound = lb0 `Set.union` lset,
    asRbound = rb0 `Set.union` rset
  }

asUpdate :: AlphaState -> Variable -> Variable -> Maybe AlphaState
asUpdate as@(AState lb0 rb0 lmap0 rmap0) v1 v2
  | not (v1bound == v2bound) = Nothing
  | not (v1bound && v2bound) = if v1 == v2 then Just as else Nothing
  -- remaining case: v1 bound && v2 bound
  | (not (v1 `Map.member` lmap0) && not (v2 `Map.member` rmap0)) =
      Just $ as {
              asLmap = Map.insert v1 v2 lmap0,
              asRmap = Map.insert v2 v1 rmap0
            }
  | (v1 `Map.member` lmap0) =
      if (lmap0 Map.! v1) == v2
        then Just as
        else Nothing
  | otherwise = Nothing
  where v1bound = v1 `Set.member` lb0
        v2bound = v2 `Set.member` rb0

-- alphaEquivalence where we assume that all bound variables are distinct
_alphaEq :: Term -> Term -> AlphaState -> Maybe AlphaState
_alphaEq t1 t2 as =
  case (t1, t2) of
    (Term_Var name1 i1, Term_Var name2 i2)    -> asUpdate as (Var name1 i1) (Var name2 i2)
    (Term_Atom name1, Term_Atom name2)        -> if name1 == name2 then Just as else Nothing
    (Term_Comp subs1, Term_Comp subs2)        ->
      if not (length subs1 == length subs2) then Nothing
      else
      let actions = map (uncurry _alphaEq) $ zip subs1 subs2
      in foldl (>>=) (Just as) actions
    (Term_Lambda vars1 sub1, Term_Lambda vars2 sub2)  -> _alphaEq sub1 sub2 $ asAddBounds as vars1 vars2
    _                                         -> Nothing

alphaEq :: Term -> Term -> Bool
alphaEq t1 t2 =
  let t1' = termMinimizeIndices t1
      t2' = termRepel [t1] t2
  in isJust $ _alphaEq t1' t2' asInit



-- substitution (for free variables)

_substitute :: Variable -> Term -> Term -> Term
_substitute v t tgt =
  case tgt of
    Term_Var name i       -> if (v == Var name i) then t else tgt
    Term_Atom _           -> tgt
    Term_Comp subs        -> Term_Comp $ map (_substitute v t) subs
    Term_Lambda vars sub  ->
      if v `Set.member` vars
        then tgt
        else Term_Lambda vars $ _substitute v t sub

substitute :: Variable -> Term -> Term -> Term
substitute v t tgt = termMergeLambdas $ _substitute v t (termRepel [t] tgt)

termSub :: Term -> Variable -> Term -> Term
termSub tgt v t = substitute v t tgt

_substituteMany :: Map.Map Variable Term -> Term -> Term
_substituteMany mapping tgt =
  case tgt of
    Term_Var name i       -> Map.findWithDefault tgt (Var name i) mapping
    Term_Atom _           -> tgt
    Term_Comp subs        -> Term_Comp $ map (substituteMany mapping) subs
    Term_Lambda vars sub  ->
      let mapping1 = Set.foldr Map.delete mapping vars
      in Term_Lambda vars $ substituteMany mapping1 sub

substituteMany :: Map.Map Variable Term -> Term -> Term
substituteMany mapping tgt = termMergeLambdas $ _substituteMany mapping (termRepel (Map.elems mapping) tgt)

termSubMany :: Term -> Map.Map Variable Term -> Term
termSubMany = flip substituteMany

{-

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

-}


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

term8 =
  Term_Lambda (Set.fromList [Var "X" 0, Var "Y" 2]) $
    Term_Comp [
      Term_Var "X" 0,
      Term_Var "Y" 2,
      Term_Lambda (Set.fromList [Var "Y" 2])$
        Term_Comp [
          Term_Atom "ahoj",
          Term_Var "Y" 2,
          Term_Var "Z" 0
        ]
    ]

term9 =
  Term_Lambda (Set.fromList [Var "X" 0, Var "Y" 2]) $
    Term_Comp [
      Term_Var "X" 0,
      Term_Var "Y" 2,
      Term_Lambda (Set.fromList [Var "Z" 4]) $
        Term_Comp [
          Term_Atom "ahoj",
          Term_Var "Z" 4,
          Term_Var "Z" 0
        ]
    ]

term10 =
  Term_Lambda (Set.fromList [Var "X" 0, Var "Y" 2]) $
    Term_Comp [
      Term_Atom "haha",
      Term_Var "Y" 2,
      Term_Lambda (Set.fromList [Var "Z" 4]) $
        Term_Comp [
          Term_Atom "ahoj",
          Term_Var "Z" 4,
          Term_Var "Z" 0
        ]
    ]

term11 =
  Term_Lambda (Set.fromList [Var "X" 0, Var "Y" 2]) $
    Term_Comp [
      Term_Var "X" 0,
      Term_Var "Y" 2,
      Term_Lambda (Set.fromList [Var "Z" 4]) $
        Term_Comp [
          Term_Atom "ahoj",
          Term_Var "Z" 4,
          Term_Atom "koniec"
        ]
    ]

term12 =
  Term_Lambda (Set.fromList [Var "X" 0, Var "Y" 2, Var "Z" 0]) $
    Term_Comp [
      Term_Var "X" 0,
      Term_Var "Y" 2,
      Term_Lambda (Set.fromList [Var "Y" 2])$
        Term_Comp [
          Term_Atom "ahoj",
          Term_Var "Y" 2,
          Term_Var "Z" 0
        ]
    ]
