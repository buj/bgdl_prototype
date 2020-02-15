import Control.Monad
import Data.List.Index
import Data.Maybe
import Data.Tuple
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set



-- basic definitions

data Atom = Atom { atomId :: String } deriving (Eq, Ord)
data Variable = Var { varName :: String, varIndex :: Int } deriving (Eq, Ord)
data Term = Term_Var String Int | Term_Atom String | Term_Comp [Term]


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
substitute v@(Var name i) t tgt =
  case tgt of
    Term_Var name2 i2 -> if (name == name2  && i == i2) then t else tgt
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


shiftVar :: String -> Int -> VarCollection -> VarCollection
shiftVar name d = Map.adjust (Set.map (+ d)) name

vcShift :: VarCollection -> String -> Int -> VarCollection
vcShift vc name i = shiftVar name i vc

shiftManyVars :: Map.Map String Int -> VarCollection -> VarCollection
shiftManyVars dmap vc = Map.foldrWithKey shiftVar vc dmap

vcShiftMany :: VarCollection -> Map.Map String Int -> VarCollection
vcShiftMany = flip shiftManyVars

vcVarMaxIndices :: VarCollection -> Map.Map String Int
vcVarMaxIndices = Map.map (fromJust . Set.lookupMax)


setIntGetCompressLs :: Set.Set Int -> [(Int, Int)]
setIntGetCompressLs s = map swap (indexed $ Set.elems s)

vcGetCompressMap :: VarCollection -> Map.Map Variable Variable
vcGetCompressMap vc =
  let ls1 = Map.assocs $ Map.map setIntGetCompressLs vc
      ls2 = ls1 >>= (\(name, iis) -> map (\(i, j) -> (Var name i, Var name j)) iis)
  in Map.fromAscList ls2

termCompressVars :: Term -> Term
termCompressVars t =
  let vc = termGetVc t
      mapping = Map.map varToTerm $ vcGetCompressMap vc
  in substituteMany mapping t



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
    ]
  ]
