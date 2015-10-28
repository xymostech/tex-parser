{-# LANGUAGE TemplateHaskell
           , Rank2Types #-}
module TeX.State
( TeXState(), mkState
, stateCategory, stateDefinition, stateCount, stateAlias
, globalStateCategory, globalStateDefinition, globalStateCount, globalStateAlias
, pushState, popState
)
where

import Control.Lens ( Lens', Setter'
                    , sets, lens, over, makeLenses
                    , (^.), (.~)
                    )

import TeX.Alias
import TeX.Category
import TeX.Count
import TeX.Def
import TeX.Register
import TeX.Token

data LocalTeXState = LocalTeXState
  { _categoryMap :: CategoryMap
  , _definitionMap :: DefinitionMap
  , _countMap :: RegisterMap Count
  , _aliasMap :: AliasMap
  }
  deriving (Show)
makeLenses ''LocalTeXState

-- We manage things like `\global` assignments by keeping track of a list of
-- the current state at different depth levels, and when applying a `\global`
-- assignment we apply it to every state in the list.
data TeXState = TeXState
  { _currState :: LocalTeXState
  , depthList :: [LocalTeXState]
  }
  deriving (Show)
makeLenses ''TeXState

mkState :: CategoryMap -> TeXState
mkState catMap =
  TeXState
  { _currState = LocalTeXState
                 { _categoryMap = catMap
                 , _definitionMap = emptyDefMap
                 , _countMap = emptyRegMap 0
                 , _aliasMap = emptyAliasMap
                 }
  , depthList = []
  }

globalStates :: Setter' TeXState LocalTeXState
globalStates =
  sets globalSet
  where
    globalSet :: (LocalTeXState -> LocalTeXState) -> TeXState -> TeXState
    globalSet f state =
      TeXState
      { _currState = f $ _currState state
      , depthList = map f $ depthList state
      }

stateAccessorWithDefault :: b ->
                            Lens' a (Maybe b) ->
                            Lens' LocalTeXState a ->
                            Lens' TeXState b
stateAccessorWithDefault defaultVal inner accessor =
  lens myView (flip mySet)
  where
    maybeLens :: Lens' a (Maybe b) -> Lens' LocalTeXState a -> Lens' TeXState (Maybe b)
    maybeLens inner' accessor' = currState . accessor' . inner'

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

globalSetterWithDefault :: b ->
                           Lens' a (Maybe b) ->
                           Lens' LocalTeXState a ->
                           Setter' TeXState b
globalSetterWithDefault defaultVal inner accessor =
  sets mySet
  where
    maybeLens :: Lens' a1 (Maybe b1) -> Lens' LocalTeXState a1 -> Setter' TeXState (Maybe b1)
    maybeLens inner' accessor' = globalStates . accessor' . inner'

    runMaybeSet :: (a2 -> b2) -> a2 -> Maybe a2 -> Maybe b2
    runMaybeSet f _ (Just val) = Just $ f val
    runMaybeSet f defaultVal' (Nothing) = Just $ f defaultVal'

    makeSet :: b3 -> Setter' TeXState (Maybe b3) -> (b3 -> b3) -> TeXState -> TeXState
    makeSet defaultVal' myLens f state =
      over myLens (runMaybeSet f defaultVal') state

    mySet = makeSet defaultVal (maybeLens inner accessor)

stateAccessor :: Lens' a (Maybe b) -> Lens' LocalTeXState a ->
                 Lens' TeXState (Maybe b)
stateAccessor inner accessor =
  currState . accessor . inner

globalSetter :: Lens' a (Maybe b) -> Lens' LocalTeXState a ->
                Setter' TeXState (Maybe b)
globalSetter inner accessor =
  globalStates . accessor . inner

stateCategory :: Char -> Lens' TeXState Category
stateCategory c = stateAccessorWithDefault Other (category c) categoryMap

globalStateCategory :: Char -> Setter' TeXState Category
globalStateCategory c = globalSetterWithDefault Other (category c) categoryMap

stateDefinition :: [Char] -> Lens' TeXState (Maybe Def)
stateDefinition name = stateAccessor (definition name) definitionMap

globalStateDefinition :: [Char] -> Setter' TeXState (Maybe Def)
globalStateDefinition name = globalSetter (definition name) definitionMap

stateCount :: Int -> Lens' TeXState (Maybe Count)
stateCount index = stateAccessor (register index) countMap

globalStateCount :: Int -> Setter' TeXState (Maybe Count)
globalStateCount index = globalSetter (register index) countMap

stateAlias :: Token -> Lens' TeXState (Maybe Alias)
stateAlias tok = stateAccessor (aliasLens tok) aliasMap

globalStateAlias :: Token -> Setter' TeXState (Maybe Alias)
globalStateAlias tok = globalSetter (aliasLens tok) aliasMap

pushState :: TeXState -> TeXState
pushState state =
  TeXState
  { _currState = _currState state
  , depthList = (_currState state):(depthList state)
  }

popState :: TeXState -> TeXState
popState (TeXState { depthList = [] }) = error "Can't pop the state more than was pushed"
popState (TeXState { depthList = (topState:restStates) }) =
  TeXState
  { _currState = topState
  , depthList = restStates
  }
