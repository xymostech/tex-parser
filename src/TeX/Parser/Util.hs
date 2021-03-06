{-# LANGUAGE Rank2Types #-}

module TeX.Parser.Util
( unimplemented
, optionalSpace, optionalSpaces
, equals
, number, eightBitNumber, count
, integerVariable, IntegerVariable(IntegerParameter, CountDefToken, LiteralCount)
, aliasFor
)
where

import Control.Lens ((^.))
import Text.Parsec ( (<|>), (<?>)
                   , choice, anyToken, getState, lookAhead
                   )

import TeX.Alias
import TeX.Category
import TeX.Count
import TeX.Parser.Parser
import TeX.Parser.Prim
import TeX.State
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

internalInteger :: Expander -> TeXParser Integer
internalInteger expand =
  variableValue
  where
    variableValue = do
      var <- integerVariable expand
      value <- integerVariableValue var
      return $ toInteger value

integerConstant :: Expander -> TeXParser Integer
integerConstant expand = do
  (value, level) <- recInteger
  if level == 0
  then fail "no numbers read"
  else return value
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
  internalInteger expand <|>
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
  deriving (Show, Eq)

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

integerVariableValue :: IntegerVariable -> TeXParser Count
integerVariableValue (IntegerParameter _) = unimplemented
integerVariableValue (CountDefToken _) = unimplemented
integerVariableValue (LiteralCount counter) = do
  val <- (^.) <$> getState <*> (return (stateCount (fromInteger counter)))
  case val of
    Just a -> return a
    Nothing -> fail $ "invalid counter: " ++ (show counter)

aliasFor :: Alias -> TeXParser Token
aliasFor alias = do
  tok <- lookAhead anyToken
  maybeAlias <- (^.) <$> getState <*> (return $ stateAlias tok)
  if maybeAlias == Just alias
  then anyToken
  else fail (show tok ++ " is not an alias for " ++ show alias)
