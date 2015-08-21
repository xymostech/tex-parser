{-# LANGUAGE TemplateHaskell
           , Rank2Types #-}
module TeX.State
( TeXState(), mkState
, stateCategory, stateDefinition, stateCount
)
where

import Control.Lens (Lens', lens, makeLenses, (^.), (.~))

import TeX.Category
import TeX.Def
import TeX.Register
import TeX.Count

data TeXState = TeXState
  { _categoryMap :: CategoryMap
  , _definitionMap :: DefinitionMap
  , _countMap :: RegisterMap Count
  }
  deriving (Show)
makeLenses ''TeXState

mkState :: CategoryMap -> TeXState
mkState catMap = TeXState
                 { _categoryMap = catMap
                 , _definitionMap = emptyDefMap
                 , _countMap = emptyRegMap 0
                 }

stateAccessorWithDefault :: b -> Lens' a (Maybe b) -> Lens' TeXState a ->
                            Lens' TeXState b
stateAccessorWithDefault defaultVal inner accessor =
  lens myView (flip $ mySet)
  where
    maybeLens :: Lens' a (Maybe b) -> Lens' TeXState a -> Lens' TeXState (Maybe b)
    maybeLens inner' accessor' = accessor' . inner'

    makeView :: b -> Lens' TeXState (Maybe b) -> TeXState -> b
    makeView defaultVal' myLens state =
      case state ^. myLens of
        Just val -> val
        Nothing -> defaultVal'

    makeSet :: Lens' TeXState (Maybe b) -> b -> TeXState -> TeXState
    makeSet myLens newVal state =
      (myLens .~ Just newVal) state

    myView = makeView defaultVal (maybeLens inner accessor)
    mySet = makeSet (maybeLens inner accessor)

stateAccessor :: Lens' a (Maybe b) -> Lens' TeXState a ->
                 Lens' TeXState (Maybe b)
stateAccessor inner accessor = accessor . inner

stateCategory :: Char -> Lens' TeXState Category
stateCategory c = stateAccessorWithDefault Other (category c) categoryMap

stateDefinition :: [Char] -> Lens' TeXState (Maybe Def)
stateDefinition name = stateAccessor (definition name) definitionMap

stateCount :: Int -> Lens' TeXState (Maybe Count)
stateCount index = stateAccessor (register index) countMap
