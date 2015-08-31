module TeX.Parser.Assignment
where

import Text.Parsec ((<|>), modifyState)
import Control.Lens ((.~))

import TeX.Def hiding (definition)
import TeX.Parser.Expand
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

macroAssignment :: TeXParser ()
macroAssignment =
  (expand definition >>= doSet)
   <|> (prefix >> macroAssignment)
  where
    doSet def@(Def name _ _) = modifyState (stateDefinition name .~ Just def)

arithmetic :: TeXParser ()
arithmetic = unimplemented

integerVariableAssignment :: TeXParser ()
integerVariableAssignment = do
  variable <- integerVariable
  equals
  value <- count
  case variable of
    IntegerParameter _ -> unimplemented
    CountDefToken _ -> unimplemented
    LiteralCount counter ->
      modifyState (stateCount (fromInteger counter) .~ Just value)

variableAssignment :: TeXParser ()
variableAssignment =
  integerVariableAssignment

simpleAssignment :: TeXParser ()
simpleAssignment =
  variableAssignment <|> arithmetic

nonMacroAssignment :: TeXParser ()
nonMacroAssignment =
  simpleAssignment <|> (expand (exactToken (ControlSequence "global")) >> nonMacroAssignment)

assignment :: TeXParser ()
assignment = nonMacroAssignment <|> macroAssignment
