module MyTerms where

import Control.Monad
import Control.Monad.Fix
import Data.List.Index
import Data.Maybe
import Data.Ord
import Data.Tuple
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Sequence as Seq

import qualified MyUtil as Mutil



-- basic definitions

data Atom = Atom { atomId :: String } deriving (Eq, Ord)
data Variable = Var { varName :: String, varIndex :: Int } deriving (Eq, Ord)
data Term =
  Term_Var String Int |
  Term_Atom String |
  Term_Comp [Term] |
  Term_Lambda (Set.Set Variable) Term


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



-- convenience functions (for fast typing)

varnum :: Int -> Variable
varnum i = Var "X" i

varstr :: String -> Variable
varstr s = Var s 0

tvarnum :: Int -> Term
tvarnum = varToTerm . varnum

tvarstr :: String -> Term
tvarstr = varToTerm . varstr

tatom :: String -> Term
tatom = Term_Atom

tcomp :: [Term] -> Term
tcomp = Term_Comp



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

termClosure :: Term -> Term
termClosure t
  | (Set.size fvars == 0) = t
  | otherwise = Term_Lambda fvars t
  where fvars = termFreeVars t

termIsGround :: Term -> Bool
termIsGround t = Set.size (termFreeVars t) == 0

termOnlyGround :: Term -> Maybe Term
termOnlyGround t
  | (termIsGround t) = Just t
  | otherwise = Nothing

type TermKey = [Maybe Term]

termKey :: Term -> TermKey
termKey (Term_Comp subs) = map termOnlyGround subs
termKey _ = []

_tkSubs :: TermKey -> [TermKey]
_tkSubs (Nothing:tail) = map (Nothing:) $ _tkSubs tail
_tkSubs (t:tail) = _tkSubs tail >>= (\key -> [Nothing:key, t:key])
_tkSubs _ = [[]]

tkSubs :: TermKey -> [TermKey]
tkSubs [] = [[]]
tkSubs key = []:(_tkSubs key)

termTkSubs :: Term -> [TermKey]
termTkSubs = tkSubs . termKey


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
termNormalize = termMinimizeIndices . termMergeLambdas

termRepel :: [Term] -> Term -> Term
termRepel ts tgt = tsMinimizeIndices (vbToTs $ getFirstUnusedIndices (Set.unions $ map termFreeVars (tgt:ts))) tgt



-- substitution

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
    Term_Comp subs        -> Term_Comp $ map (_substituteMany mapping) subs
    Term_Lambda vars sub  ->
      let mapping1 = Set.foldr Map.delete mapping vars
      in Term_Lambda vars $ _substituteMany mapping1 sub

substituteMany :: Map.Map Variable Term -> Term -> Term
substituteMany mapping tgt = termMergeLambdas $ _substituteMany  mapping (termRepel (Map.elems mapping) tgt)

termSubMany :: Term -> Map.Map Variable Term -> Term
termSubMany = flip substituteMany



-- canonical form of terms

_massRename :: Map.Map Variable Variable -> Term -> Term
_massRename mapping tgt =
  case tgt of
    Term_Var name i       -> varToTerm $ Map.findWithDefault v v mapping where v = Var name i
    Term_Atom _           -> tgt
    Term_Comp subs        -> Term_Comp $ map (_massRename mapping) subs
    Term_Lambda vars sub  ->
      let vars1 = Set.map (\v -> Map.findWithDefault v v mapping) vars
      in Term_Lambda vars1 $ _massRename mapping sub

collectBoundVars :: [Variable] -> Term -> [Variable]
collectBoundVars ls t =
  case t of
    Term_Lambda vars sub  -> collectBoundVars (Set.elems vars ++ ls) sub
    Term_Comp subs        -> foldl collectBoundVars ls subs
    _                     -> ls

termCanonMap :: Term -> Map.Map Variable Variable
termCanonMap t =
  let varList = collectBoundVars [] t
      tmp1 = map swap $ indexed varList
      tmp2 = map (\(v, i) -> (v, varnum i)) tmp1
  in Map.fromList tmp2

termCanon :: Term -> Term
termCanon t = _massRename (termCanonMap t') t' where t' = termNormalize t



-- term ordering and Eq (alpha equivalence / ordering)

termLevel :: Term -> Int
termLevel (Term_Var _ _) = 0
termLevel (Term_Atom _) = 1
termLevel (Term_Comp _) = 2
termLevel (Term_Lambda _ _) = 3

termSimpleCmp :: Term -> Term -> Ordering
termSimpleCmp t1 t2
  | let tLv1 = termLevel t1
        tLv2 = termLevel t2,
    not (tLv1 == tLv2) = tLv1 `compare` tLv2
  | otherwise =
      case (t1, t2) of
        (Term_Var name1 i1, Term_Var name2 i2)              -> (name1, i1) `compare` (name2, i2)
        (Term_Atom name1, Term_Atom name2)                  -> name1 `compare` name2
        (Term_Comp subs1, Term_Comp subs2)                  -> subs1 `compare` subs2
        (Term_Lambda vars1 subs1, Term_Lambda vars2 subs2)  -> (vars1, subs1) `compare` (vars2, subs2)

instance Eq Term where
  (==) t1 t2 = (termSimpleCmp (termCanon t1) (termCanon t2) == EQ)

instance Ord Term where
  compare t1 t2 = termSimpleCmp (termCanon t1) (termCanon t2)



-- pattern matching

data PmState = PmState {
  pmFvars :: VarSet,
  pmMapping :: Map.Map Variable Term
}

pmInit :: VarSet -> PmState
pmInit vars = PmState vars Map.empty

updatePm :: Variable -> Term -> PmState -> Maybe PmState
updatePm v t pm@(PmState fvars0 mapping0)
  | not (v `Set.member` fvars0) = Just pm
  | not (v `Map.member` mapping0) = Just $ pm { pmMapping = Map.insert v t mapping0 }
  | otherwise =
      let t2 = mapping0 Map.! v
      in if t == t2 then Just pm else Nothing

_patternMatch :: Term -> Term -> PmState -> Maybe PmState
_patternMatch (Term_Var name1 i1) t2 = updatePm (Var name1 i1) t2
_patternMatch (Term_Atom name1) (Term_Atom name2)
  | (name1 == name2) = Just
  | otherwise = const Nothing
_patternMatch (Term_Comp subs1) (Term_Comp subs2)
  | (length subs1 == length subs2) =
      \pm -> foldl (>>=) (Just pm) $ map (uncurry _patternMatch) (zip subs1 subs2)
  | otherwise = const Nothing
_patternMatch (Term_Lambda vars1 sub1) (Term_Lambda vars2 sub2)
  | (Set.size vars1 == Set.size vars2) = _patternMatch sub1 sub2
  | otherwise = const Nothing
_patternMatch _ _ = const Nothing

patternMatch :: Term -> Term -> Maybe (Map.Map Variable Term)
patternMatch t1 t2 = do
  pm <- _patternMatch t1 t2 (pmInit $ termFreeVars t1)
  let mapping = pmMapping pm
      t1' = termSubMany t1 mapping
  if t1' == t2
    then Just mapping
    else Nothing

patternMatch_safe t1 t2 = patternMatch (termNormalize t1) (termNormalize t2)
