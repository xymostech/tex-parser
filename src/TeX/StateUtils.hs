{-# LANGUAGE ScopedTypeVariables, Rank2Types #-}
module TeX.StateUtils
where

import Prelude (Ord, Maybe(Just, Nothing), (.), flip)
import Control.Lens (Lens', lens)
import qualified Data.Map as M

makeLens :: Ord k => (a -> M.Map k v) -> (M.Map k v -> a) -> k -> Lens' a (Maybe v)
makeLens from to key =
  lens view (flip set)
  where
    --view :: a -> Maybe v
    view = (M.lookup key) . from

    --set :: Maybe v -> a -> a
    set (Just val) = to . (M.insert key val) . from
    set Nothing = to . (M.delete key) . from
