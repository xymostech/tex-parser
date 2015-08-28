module TeX.Parser.Assignment
where

import Text.Parsec ((<|>), (<?>), modifyState, choice)
import Control.Lens ((.~))

import TeX.Def hiding (definition)
import TeX.Parser.Parser
import TeX.Parser.Prim
import TeX.Parser.MacroParser
import TeX.Token
import TeX.State
import TeX.Category
import TeX.Count

unimplemented :: TeXParser a
unimplemented = fail "not implemented"

definition :: TeXParser Def
definition = parseDef

prefix :: TeXParser Token
prefix = unimplemented

macroAssignment :: TeXParser ()
macroAssignment =
  (definition >>= doSet)
   <|> (prefix >> macroAssignment)
  where
    doSet def@(Def name _ _) = modifyState (stateDefinition name .~ Just def)

arithmetic :: TeXParser ()
arithmetic = unimplemented

optionalSpaces :: TeXParser ()
optionalSpaces =
  (categoryToken Space >> optionalSpaces) <|> (return ())

optionalSpace :: TeXParser ()
optionalSpace =
  (categoryToken Space >> return ()) <|> (return ())

equals :: TeXParser ()
equals =
  (optionalSpaces >> exactToken (CharToken '=' Other) >> return ()) <|>
  optionalSpaces

plusOrMinus :: TeXParser Integer
plusOrMinus =
  (exactToken (CharToken '+' Other) >> return 1) <|>
  (exactToken (CharToken '-' Other) >> return (-1)) <?>
  "plus or minus"

optionalSigns :: TeXParser Integer
optionalSigns =
  (((*) <$> plusOrMinus <*> optionalSigns) <* optionalSpaces) <|>
  (optionalSpaces >> return 1) <?>
  "optional sign"

internalInteger :: TeXParser Integer
internalInteger = unimplemented

integerConstant :: TeXParser Integer
integerConstant =
  fst <$> recInteger
  where
    digit = choice [exactToken (CharToken x Other) | x <- ['0'..'9']]

    recInteger :: TeXParser (Integer, Integer)
    recInteger =
      (do
        Just d <- charCode <$> digit
        (rest, level) <- recInteger
        return $ (d * 10 ^ level + rest, level + 1)) <|>
      (return (0, 0))

hexadecimalConstant :: TeXParser Integer
hexadecimalConstant = unimplemented

octalConstant :: TeXParser Integer
octalConstant = unimplemented

characterToken :: TeXParser Integer
characterToken = unimplemented

normalInteger :: TeXParser Integer
normalInteger =
  internalInteger <|>
  (integerConstant <* optionalSpace) <|>
  (exactToken (CharToken '\'' Other) >> octalConstant <* optionalSpace) <|>
  (exactToken (CharToken '"' Other) >> hexadecimalConstant <* optionalSpace) <|>
  (exactToken (CharToken '`' Other) >> characterToken <* optionalSpace) <?>
  "normal integer"

coercedInteger :: TeXParser Integer
coercedInteger = unimplemented

unsignedNumber :: TeXParser Integer
unsignedNumber = normalInteger <|> coercedInteger <?> "unsigned number"

number :: TeXParser Integer
number = (*) <$> optionalSigns <*> unsignedNumber

data IntegerVariable =
  IntegerParameter String |
  CountDefToken String |
  LiteralCount Integer

eightBitNumber :: TeXParser Integer
eightBitNumber = do
  value <- number
  if value < 0 || value >= 256
  then fail $ "number too large: " ++ (show value)
  else return value

integerVariable :: TeXParser IntegerVariable
integerVariable =
  integerParameter <|>
  countDefToken <|>
  literalCount <?>
  "integer variable"
  where
    integerParameter = unimplemented
    countDefToken = unimplemented
    literalCount =
      LiteralCount <$> (exactToken (ControlSequence "count") >> eightBitNumber)

integerVariableAssignment :: TeXParser ()
integerVariableAssignment = do
  variable <- integerVariable
  equals
  value <- number
  case fromInteger $ value of
    CountOverflow -> fail "number too large"
    count ->
      case variable of
        IntegerParameter _ -> unimplemented
        CountDefToken _ -> unimplemented
        LiteralCount counter ->
          modifyState (stateCount (fromInteger counter) .~ Just count)

variableAssignment :: TeXParser ()
variableAssignment =
  integerVariableAssignment

simpleAssignment :: TeXParser ()
simpleAssignment =
  variableAssignment <|> arithmetic

nonMacroAssignment :: TeXParser ()
nonMacroAssignment =
  simpleAssignment <|> (exactToken (ControlSequence "global") >> nonMacroAssignment)

assignment :: TeXParser ()
assignment = nonMacroAssignment <|> macroAssignment
