{-# LANGUAGE ScopedTypeVariables, Rank2Types #-}
module TeX.StateUtils
where

import Prelude (Ord, Maybe(Just, Nothing), Int, (.), flip, ($))
import Control.Lens (Lens', lens)
import qualified Data.Map as M
import qualified Data.Vector as V

makeMapLens :: Ord k => (a -> M.Map k v) -> (M.Map k v -> a) -> k -> Lens' a (Maybe v)
makeMapLens from to key =
  lens view (flip set)
  where
    --view :: a -> Maybe v
    view = (M.lookup key) . from

    --set :: Maybe v -> a -> a
    set (Just val) = to . (M.insert key val) . from
    set Nothing = to . (M.delete key) . from

makeVectorLens :: (a -> V.Vector v) -> (V.Vector v -> a) -> Int -> Lens' a (Maybe v)
makeVectorLens from to key =
  lens view (flip set)
  where
    --view :: a -> Maybe v
    view t = (V.!?) (from t) key

    set (Just val) t = to $ (V.//) (from t) [(key, val)]
    set Nothing t = t
