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

definition :: TeXParser Def
definition = parseDef

prefix :: TeXParser Token
prefix = unimplemented

macroAssignment :: Expander -> TeXParser ()
macroAssignment expand =
  (expand definition >>= doSet)
   <|> (prefix >> macroAssignment expand)
  where
    doSet def@(Def name _ _) = modifyState (stateDefinition name .~ Just def)

setIntegerVariable :: IntegerVariable -> Maybe Count -> TeXParser ()
setIntegerVariable (IntegerParameter _) _ = unimplemented
setIntegerVariable (CountDefToken _) _ = unimplemented
setIntegerVariable (LiteralCount counter) value =
  modifyState (stateCount (fromInteger counter) .~ value)

modifyIntegerVariable :: IntegerVariable -> (Count -> Count) -> TeXParser ()
modifyIntegerVariable (IntegerParameter _) _  = unimplemented
modifyIntegerVariable (CountDefToken _) _ = unimplemented
modifyIntegerVariable var@(LiteralCount counter) modify = do
  currValue <- (^.) <$> getState <*> (return (stateCount (fromInteger counter)))
  setIntegerVariable var (currValue >>= (return . modify))

arithmetic :: Expander -> TeXParser ()
arithmetic expand =
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
      modifyIntegerVariable variable (\x -> x + value)

    multiply = do
      _ <- expand (exactToken (ControlSequence "multiply"))
      variable <- integerVariable expand
      optionalBy
      value <- count expand
      modifyIntegerVariable variable (\x -> x * value)

    divide = do
      _ <- expand (exactToken (ControlSequence "divide"))
      variable <- integerVariable expand
      optionalBy
      value <- count expand
      modifyIntegerVariable variable (\x -> x `div` value)

integerVariableAssignment :: Expander -> TeXParser ()
integerVariableAssignment expand = do
  variable <- integerVariable expand
  equals expand
  value <- count expand
  setIntegerVariable variable (Just value)

variableAssignment :: Expander -> TeXParser ()
variableAssignment expand =
  integerVariableAssignment expand

simpleAssignment :: Expander -> TeXParser ()
simpleAssignment expand =
  variableAssignment expand <|> arithmetic expand

nonMacroAssignment :: Expander -> TeXParser ()
nonMacroAssignment expand =
  simpleAssignment expand <|> (expand (exactToken (ControlSequence "global")) >> nonMacroAssignment expand)

assignment :: Expander -> TeXParser ()
assignment expand = nonMacroAssignment expand <|> macroAssignment expand
