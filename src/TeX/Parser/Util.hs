module TeX.Parser.Util
( unimplemented
, optionalSpace, optionalSpaces
, equals
, number, eightBitNumber, count
, integerVariable, IntegerVariable(IntegerParameter, CountDefToken, LiteralCount)
)
where

import Text.Parsec ((<|>), (<?>), choice)

import TeX.Category
import TeX.Count
import TeX.Parser.Expand
import TeX.Parser.Parser
import TeX.Parser.Prim
import TeX.Token

unimplemented :: TeXParser a
unimplemented = fail "not implemented"

optionalSpaces :: TeXParser ()
optionalSpaces =
  (expand (categoryToken Space) >> optionalSpaces) <|>
  (return ()) <?>
  "optional spaces"

optionalSpace :: TeXParser ()
optionalSpace =
  (expand (categoryToken Space) >> return ()) <|>
  (return ()) <?>
  "optional space"

equals :: TeXParser ()
equals =
  (optionalSpaces >> expand (exactToken (CharToken '=' Other)) >> return ()) <|>
  optionalSpaces <?>
  "optional equals"

plusOrMinus :: TeXParser Integer
plusOrMinus =
  (expand (exactToken (CharToken '+' Other)) >> return 1) <|>
  (expand (exactToken (CharToken '-' Other)) >> return (-1)) <?>
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
  fst <$> recInteger <?> "integer constant"
  where
    digit = expand $ choice [exactToken (CharToken x Other) | x <- ['0'..'9']]

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
  (expand (exactToken (CharToken '\'' Other)) >> octalConstant <* optionalSpace) <|>
  (expand (exactToken (CharToken '"' Other)) >> hexadecimalConstant <* optionalSpace) <|>
  (expand (exactToken (CharToken '`' Other)) >> characterToken <* optionalSpace) <?>
  "normal integer"

coercedInteger :: TeXParser Integer
coercedInteger = unimplemented

unsignedNumber :: TeXParser Integer
unsignedNumber = normalInteger <|> coercedInteger <?> "unsigned number"

number :: TeXParser Integer
number = (*) <$> optionalSigns <*> unsignedNumber <?> "number"

eightBitNumber :: TeXParser Integer
eightBitNumber = do
  value <- number
  if value < 0 || value >= 256
  then fail $ "number too large: " ++ (show value)
  else return value

count :: TeXParser Count
count = do
  value <- fromInteger <$> number
  case value of
    CountOverflow -> fail "number too large"
    _ -> return value

data IntegerVariable =
  IntegerParameter String |
  CountDefToken String |
  LiteralCount Integer

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
      LiteralCount <$> (expand (exactToken (ControlSequence "count")) >> eightBitNumber)
