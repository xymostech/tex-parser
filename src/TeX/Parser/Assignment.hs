module TeX.Parser.Assignment
( assignment
)
where

import Text.Parsec ((<|>), modifyState)
import Control.Lens ((.~))

import TeX.Parser.Parser
import TeX.Def hiding (definition)
import TeX.Parser.MacroParser
import TeX.Token
import TeX.State

definition :: TeXParser Def
definition = parseDef

prefix :: TeXParser Token
prefix = fail "not implemented"

macroAssignment :: TeXParser ()
macroAssignment =
  (definition >>= doSet)
   <|> (prefix >> macroAssignment)
  where
    doSet def@(Def name _ _) = modifyState (stateDefinition name .~ Just def)

nonMacroAssignment :: TeXParser ()
nonMacroAssignment = fail "not implemented"

assignment :: TeXParser ()
assignment = nonMacroAssignment <|> macroAssignment
