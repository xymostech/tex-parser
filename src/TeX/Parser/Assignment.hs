{-# LANGUAGE Rank2Types #-}
module TeX.Parser.Assignment
where

import Text.Parsec ((<|>), (<?>), modifyState, getState)
import Control.Lens ((.~), (^.))

import TeX.Category
import TeX.Count
import TeX.Def hiding (definition)
import TeX.Parser.MacroParser
import TeX.Parser.Parser
import TeX.Parser.Prim
import TeX.Parser.Util
import TeX.State
import TeX.Token

definition :: Expander -> TeXParser Def
definition = parseDef

prefix :: TeXParser Token
prefix = unimplemented

macroAssignment :: Expander -> TeXParser ()
macroAssignment expand =
  (definition expand >>= doSet)
   <|> (prefix >> macroAssignment expand)
  where
    doSet def@(Def name _ _) = modifyState (stateDefinition name .~ Just def)

setIntegerVariable :: Bool -> IntegerVariable -> Maybe Count -> TeXParser ()
setIntegerVariable _ (IntegerParameter _) _ = unimplemented
setIntegerVariable _ (CountDefToken _) _ = unimplemented
setIntegerVariable True (LiteralCount counter) value =
  modifyState (globalStateCount (fromInteger counter) .~ value)
setIntegerVariable False (LiteralCount counter) value =
  modifyState (stateCount (fromInteger counter) .~ value)

modifyIntegerVariable :: Bool -> IntegerVariable -> (Count -> Count) -> TeXParser ()
modifyIntegerVariable _ (IntegerParameter _) _  = unimplemented
modifyIntegerVariable _ (CountDefToken _) _ = unimplemented
modifyIntegerVariable global var@(LiteralCount counter) modify = do
  currValue <- (^.) <$> getState <*> (return (stateCount (fromInteger counter)))
  setIntegerVariable global var (currValue >>= (return . modify))

arithmetic :: Expander -> Bool -> TeXParser ()
arithmetic expand global =
  advance <|> multiply <|> divide
  where
    optionalBy =
      (expand (exactToken (CharToken 'b' Letter)) >>
       expand (exactToken (CharToken 'y' Letter)) >>
       return ()) <|> (return ()) <?> "optional by"

    -- TODO(emily): make this work for dimen/glue/muglue
    advance = do
      _ <- expand (exactToken (ControlSequence "advance"))
      variable <- integerVariable expand
      optionalBy
      value <- count expand
      modifyIntegerVariable global variable (\x -> x + value)

    multiply = do
      _ <- expand (exactToken (ControlSequence "multiply"))
      variable <- integerVariable expand
      optionalBy
      value <- count expand
      modifyIntegerVariable global variable (\x -> x * value)

    divide = do
      _ <- expand (exactToken (ControlSequence "divide"))
      variable <- integerVariable expand
      optionalBy
      value <- count expand
      modifyIntegerVariable global variable (\x -> x `div` value)

integerVariableAssignment :: Expander -> Bool -> TeXParser ()
integerVariableAssignment expand global = do
  variable <- integerVariable expand
  equals expand
  value <- count expand
  setIntegerVariable global variable (Just value)

variableAssignment :: Expander -> Bool -> TeXParser ()
variableAssignment expand global =
  integerVariableAssignment expand global

simpleAssignment :: Expander -> Bool -> TeXParser ()
simpleAssignment expand global =
  variableAssignment expand global <|> arithmetic expand global

nonMacroAssignment :: Expander -> Bool -> TeXParser ()
nonMacroAssignment expand global =
  simpleAssignment expand global <|> (expand (exactToken (ControlSequence "global")) >> nonMacroAssignment expand True)

assignment :: Expander -> TeXParser ()
assignment expand = nonMacroAssignment expand False <|> macroAssignment expand
