module TeX.Def
( ParameterToken(PTToken, PTParameter, PTTrailingBrace)
, ReplacementToken(RTToken, RTParameter)
, Def(Def)
, DefinitionMap(), emptyDefMap, definition
)
where

import qualified Data.Map as M

import TeX.Token
import TeX.StateUtils

data ParameterToken
  = PTToken Token
  | PTParameter Int
  | PTTrailingBrace
  deriving (Eq, Show)

data ReplacementToken
  = RTToken Token
  | RTParameter Int
  deriving (Eq, Show)

data Def = Def String [ParameterToken] [ReplacementToken]
  deriving (Eq, Show)

newtype DefinitionMap = DefinitionMap (M.Map String Def)
  deriving (Show)

emptyDefMap :: DefinitionMap
emptyDefMap = DefinitionMap $ M.empty

definition :: Functor f => String -> (Maybe Def -> f (Maybe Def)) -> DefinitionMap -> f (DefinitionMap)
definition = makeMapLens (\(DefinitionMap x) -> x) DefinitionMap
