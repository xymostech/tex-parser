{-# LANGUAGE Rank2Types #-}
module TeX.Parser.Assignment
where

import Text.Parsec ((<|>), modifyState)
import Control.Lens ((.~))

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

arithmetic :: TeXParser ()
arithmetic = unimplemented

integerVariableAssignment :: Expander -> TeXParser ()
integerVariableAssignment expand = do
  variable <- integerVariable expand
  equals expand
  value <- count expand
  case variable of
    IntegerParameter _ -> unimplemented
    CountDefToken _ -> unimplemented
    LiteralCount counter ->
      modifyState (stateCount (fromInteger counter) .~ Just value)

variableAssignment :: Expander -> TeXParser ()
variableAssignment expand =
  integerVariableAssignment expand

simpleAssignment :: Expander -> TeXParser ()
simpleAssignment expand =
  variableAssignment expand <|> arithmetic

nonMacroAssignment :: Expander -> TeXParser ()
nonMacroAssignment expand =
  simpleAssignment expand <|> (expand (exactToken (ControlSequence "global")) >> nonMacroAssignment expand)

assignment :: Expander -> TeXParser ()
assignment expand = nonMacroAssignment expand <|> macroAssignment expand
