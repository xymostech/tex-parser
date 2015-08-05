module TeX.Def where

import qualified Data.Map as M

import TeX.Token

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

type DefinitionMap = M.Map String Def

emptyDefMap :: DefinitionMap
emptyDefMap = M.empty
