{-# LANGUAGE Rank2Types #-}

module TeX.Parser.Conditional
( expandConditional

-- for tests:
, conditionalHead, evaluateHead, runConditionalBody
, NumberValue(LitCount, IntVar)
, ConditionalHead(IfTrue, IfFalse, IfNum)
, Relation(LessThan, GreaterThan, EqualTo)
)
where

import Prelude ( Maybe(Just, Nothing), Integer, Show, Eq, Bool(True, False)
               , return, fail, fromInteger, show, reverse
               , (>>), (+), (-), ($), (++), (<), (==), (>), (>>=)
               )
import Control.Applicative ((<$>), (<*))
import Text.Parsec ((<|>), (<?>), getState, anyToken, lookAhead)
import Control.Lens ((^.))

import TeX.Parser.Assignment
import TeX.Parser.Parser
import TeX.Parser.Prim
import TeX.Parser.Util
import TeX.Count
import TeX.Token
import TeX.State
import TeX.Category

data NumberValue = LitCount Count | IntVar IntegerVariable
  deriving (Show, Eq)

numberValue :: Expander -> TeXParser NumberValue
numberValue expand =
  (LitCount <$> count expand) <|>
  (IntVar <$> integerVariable expand) <?> "number value"

valueOfNumberValue :: NumberValue -> TeXParser Count
valueOfNumberValue (LitCount val) = return val
valueOfNumberValue (IntVar var) = do
  state <- getState
  case var of
    IntegerParameter _ -> unimplemented
    CountDefToken _ -> unimplemented
    LiteralCount counter ->
      case state ^. (stateCount $ fromInteger counter) of
        Nothing -> fail $ "invalid counter: " ++ (show counter)
        Just value -> return value

data Relation = LessThan | EqualTo | GreaterThan
  deriving (Show, Eq)

relation :: Expander -> TeXParser Relation
relation expand =
  (expand (exactToken (CharToken '<' Other)) >> return LessThan) <|>
  (expand (exactToken (CharToken '=' Other)) >> return EqualTo) <|>
  (expand (exactToken (CharToken '>' Other)) >> return GreaterThan) <?>
  "relation"

data ConditionalHead =
    IfNum NumberValue Relation NumberValue
  | IfTrue
  | IfFalse
  deriving (Show, Eq)

conditionalHead :: Expander -> TeXParser ConditionalHead
conditionalHead expand =
  ifnum <|> iftrue <|> iffalse
  where
    iftrue = exactToken (ControlSequence "iftrue") >> (return IfTrue)

    ifnum = do
      _ <- exactToken (ControlSequence "ifnum")
      left <- numberValue expand
      rel <- relation expand
      right <- numberValue expand
      return $ IfNum left rel right

    iffalse = exactToken (ControlSequence "iffalse") >> (return IfFalse)

evaluateHead :: ConditionalHead -> TeXParser Bool
evaluateHead (IfNum left rel right) = do
  leftVal <- valueOfNumberValue left
  rightVal <- valueOfNumberValue right
  case rel of
    LessThan -> return $ leftVal < rightVal
    EqualTo -> return $ leftVal == rightVal
    GreaterThan -> return $ leftVal > rightVal
evaluateHead IfTrue = return True
evaluateHead IfFalse = return False

ifToken :: TeXParser Token
ifToken =
  (exactToken (ControlSequence "iftrue")) <|>
  (exactToken (ControlSequence "iffalse")) <|>
  (exactToken (ControlSequence "ifnum")) <?>
  "if token"

elseToken :: TeXParser Token
elseToken =
  (exactToken (ControlSequence "else")) <?> "else token"

fiToken :: TeXParser Token
fiToken =
  (exactToken (ControlSequence "fi")) <?> "fi token"

expandUntil :: Expander -> TeXParser Token -> TeXParser [Token]
expandUntil expand stop =
  untilRec []
  where
    untilRec :: [Token] -> TeXParser [Token]
    untilRec revList =
      expand $
        (((lookAhead stop) >> (return $ reverse revList)) <|>
         (assignment expand >> untilRec revList) <|>
         (anyToken >>= (\tok -> untilRec (tok:revList))))

ignoreUntil :: TeXParser Token -> TeXParser ()
ignoreUntil stop =
  ignoreUntilRec 0
  where
    stopLevel :: Integer -> TeXParser ()
    stopLevel 0 = return ()
    stopLevel _ = fail "not at level 0"

    ignoreUntilRec :: Integer -> TeXParser ()
    ignoreUntilRec level =
      (stopLevel level >> (lookAhead stop) >> return ()) <|>
      (ifToken >> ignoreUntilRec (level + 1)) <|>
      (fiToken >> ignoreUntilRec (level - 1)) <|>
      (anyToken >> ignoreUntilRec level)

runConditionalBody :: Expander -> Bool -> TeXParser [Token]
runConditionalBody expand True = do
  body <- expandUntil expand (elseToken <|> fiToken)
  ((fiToken >> return body) <|>
   (elseToken >> ignoreUntil fiToken >> fiToken >> return body))
runConditionalBody expand False = do
  ignoreUntil (elseToken <|> fiToken)
  ((fiToken >> return []) <|>
   (elseToken >> expandUntil expand fiToken <* fiToken))

expandConditional :: Expander -> TeXParser [Token]
expandConditional expand = do
  head <- conditionalHead expand
  value <- evaluateHead head
  runConditionalBody expand value
