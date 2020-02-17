module MyUtil where

import qualified Data.Map.Strict as Map

adjustOrInsertFunc :: (a -> a) -> a -> Maybe a -> Maybe a
adjustOrInsertFunc f dfl orig =
  case orig of
    Nothing   -> Just dfl
    Just val  -> Just $ f val

adjustOrInsert :: Ord k => (a -> a) -> a -> k -> Map.Map k a -> Map.Map k a
adjustOrInsert f dfl = Map.alter (adjustOrInsertFunc f dfl)

insertIfAbsent :: Ord k => k -> a -> Map.Map k a -> Map.Map k a
insertIfAbsent = Map.insertWith (flip const)
