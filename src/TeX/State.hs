module TeX.State
( TeXState(stateCategoryMap), mkState
, getDefinition, setDefinition
)
where

import qualified Data.Map as M

import TeX.Category
import TeX.Def

data TeXState = TeXState
  { stateCategoryMap :: CategoryMap
  , stateDefinitionMap :: DefinitionMap
  }
  deriving (Show)

mkState :: CategoryMap -> TeXState
mkState catMap = TeXState catMap emptyDefMap

setDefinition :: Def -> TeXState -> TeXState
setDefinition def@(Def name _ _) state =
  state { stateDefinitionMap = M.insert name def $ stateDefinitionMap state }

getDefinition :: [Char] -> TeXState -> Maybe Def
getDefinition name state =
  M.lookup name $ stateDefinitionMap state
