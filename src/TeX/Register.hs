module TeX.Register
where

import qualified Data.Vector as V

import TeX.StateUtils

data RegisterMap regtype = RegisterMap (V.Vector regtype)
  deriving (Show)

emptyRegMap :: a -> RegisterMap a
emptyRegMap initial =
  RegisterMap $ V.generate 256 (\_ -> initial)

register :: Functor f => Int -> (Maybe a -> f (Maybe a)) ->
            RegisterMap a -> f (RegisterMap a)
register = makeVectorLens (\(RegisterMap v) -> v) RegisterMap
