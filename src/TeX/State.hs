{-# LANGUAGE TemplateHaskell
           , Rank2Types #-}
module TeX.State
( TeXState(), mkState
, stateCategory, stateDefinition
)
where

import Control.Lens

import TeX.Category
import TeX.Def

data TeXState = TeXState
  { _categoryMap :: CategoryMap
  , _definitionMap :: DefinitionMap
  }
  deriving (Show)
makeLenses ''TeXState

mkState :: CategoryMap -> TeXState
mkState catMap = TeXState catMap emptyDefMap

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
