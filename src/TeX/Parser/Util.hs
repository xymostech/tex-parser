{-# LANGUAGE Rank2Types #-}

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
import TeX.Parser.Parser
import TeX.Parser.Prim
import TeX.Token

unimplemented :: TeXParser a
unimplemented = fail "not implemented"

optionalSpaces :: Expander -> TeXParser ()
optionalSpaces expand =
  (expand (categoryToken Space) >> optionalSpaces expand) <|>
  (return ()) <?>
  "optional spaces"

optionalSpace :: Expander -> TeXParser ()
optionalSpace expand =
  (expand (categoryToken Space) >> return ()) <|>
  (return ()) <?>
  "optional space"

equals :: Expander -> TeXParser ()
equals expand =
  (optionalSpaces expand >> expand (exactToken (CharToken '=' Other)) >> return ()) <|>
  optionalSpaces expand <?>
  "optional equals"

plusOrMinus :: Expander -> TeXParser Integer
plusOrMinus expand =
  (expand (exactToken (CharToken '+' Other)) >> return 1) <|>
  (expand (exactToken (CharToken '-' Other)) >> return (-1)) <?>
  "plus or minus"

optionalSigns :: Expander -> TeXParser Integer
optionalSigns expand =
  (((*) <$> plusOrMinus expand <*> optionalSigns expand) <* optionalSpaces expand) <|>
  (optionalSpaces expand >> return 1) <?>
  "optional sign"

internalInteger :: TeXParser Integer
internalInteger = unimplemented

integerConstant :: Expander -> TeXParser Integer
integerConstant expand =
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

normalInteger :: Expander -> TeXParser Integer
normalInteger expand =
  internalInteger <|>
  (integerConstant expand <* optionalSpace expand) <|>
  (expand (exactToken (CharToken '\'' Other)) >> octalConstant <* optionalSpace expand) <|>
  (expand (exactToken (CharToken '"' Other)) >> hexadecimalConstant <* optionalSpace expand) <|>
  (expand (exactToken (CharToken '`' Other)) >> characterToken <* optionalSpace expand) <?>
  "normal integer"

coercedInteger :: TeXParser Integer
coercedInteger = unimplemented

unsignedNumber :: Expander -> TeXParser Integer
unsignedNumber expand =
    normalInteger expand <|> coercedInteger <?> "unsigned number"

number :: Expander -> TeXParser Integer
number expand =
    (*) <$> optionalSigns expand <*> unsignedNumber expand <?> "number"

eightBitNumber :: Expander -> TeXParser Integer
eightBitNumber expand = do
  value <- number expand
  if value < 0 || value >= 256
  then fail $ "number too large: " ++ (show value)
  else return value

count :: Expander -> TeXParser Count
count expand = do
  value <- fromInteger <$> number expand
  case value of
    CountOverflow -> fail "number too large"
    _ -> return value

data IntegerVariable =
  IntegerParameter String |
  CountDefToken String |
  LiteralCount Integer

integerVariable :: Expander -> TeXParser IntegerVariable
integerVariable expand =
  integerParameter <|>
  countDefToken <|>
  literalCount <?>
  "integer variable"
  where
    integerParameter = unimplemented
    countDefToken = unimplemented
    literalCount =
      LiteralCount <$> (expand (exactToken (ControlSequence "count")) >>
                               eightBitNumber expand)
