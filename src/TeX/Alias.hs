module TeX.Alias
( Alias(AliasIfTrue, AliasIfFalse)
, AliasMap(), emptyAliasMap, aliasLens
)
where

import qualified Data.Map as M

import TeX.StateUtils
import TeX.Token

data Alias = AliasIfTrue | AliasIfFalse
  deriving (Eq, Show)

newtype AliasMap = AliasMap (M.Map Token Alias)
  deriving (Show)

emptyAliasMap :: AliasMap
emptyAliasMap = AliasMap M.empty

aliasLens :: Functor f => Token -> (Maybe Alias -> f (Maybe Alias)) -> AliasMap -> f AliasMap
aliasLens = makeMapLens (\(AliasMap x) -> x) AliasMap
