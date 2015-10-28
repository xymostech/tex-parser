{-# LANGUAGE Rank2Types #-}

module TeX.Parser.Conditional
( expandConditional

-- for tests:
, conditionalHead, evaluateHead, runConditionalBody
, ConditionalHead(IfTrue, IfFalse, IfNum)
, Relation(LessThan, GreaterThan, EqualTo)
)
where

import Prelude ( Integer, Show, Eq, Bool(True, False)
               , return, fail, reverse
               , (>>), (+), (-), ($), (<), (==), (>), (>>=)
               )
import Control.Applicative ((<*))
import Text.Parsec ((<|>), (<?>), anyToken, lookAhead)

import TeX.Alias
import TeX.Parser.Assignment
import TeX.Parser.Parser
import TeX.Parser.Prim
import TeX.Parser.Util
import TeX.Token
import TeX.Category

data Relation = LessThan | EqualTo | GreaterThan
  deriving (Show, Eq)

relation :: Expander -> TeXParser Relation
relation expand =
  (expand (exactToken (CharToken '<' Other)) >> return LessThan) <|>
  (expand (exactToken (CharToken '=' Other)) >> return EqualTo) <|>
  (expand (exactToken (CharToken '>' Other)) >> return GreaterThan) <?>
  "relation"

data ConditionalHead =
    IfNum Integer Relation Integer
  | IfTrue
  | IfFalse
  deriving (Show, Eq)

conditionalHead :: Expander -> TeXParser ConditionalHead
conditionalHead expand =
  ifnum <|> iftrue <|> iffalse
  where
    iftrue =
      (exactToken (ControlSequence "iftrue") <|> aliasFor AliasIfTrue) >> (return IfTrue)

    ifnum = do
      _ <- exactToken (ControlSequence "ifnum")
      left <- number expand
      rel <- relation expand
      right <- number expand
      return $ IfNum left rel right

    iffalse =
      (exactToken (ControlSequence "iffalse") <|> aliasFor AliasIfFalse) >> (return IfFalse)

evaluateHead :: ConditionalHead -> TeXParser Bool
evaluateHead (IfNum left rel right) =
  case rel of
    LessThan -> return $ left < right
    EqualTo -> return $ left == right
    GreaterThan -> return $ left > right
evaluateHead IfTrue = return True
evaluateHead IfFalse = return False

ifToken :: TeXParser Token
ifToken =
  (exactToken (ControlSequence "iftrue")) <|>
  (aliasFor AliasIfTrue) <|>
  (exactToken (ControlSequence "iffalse")) <|>
  (aliasFor AliasIfFalse) <|>
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
      ((expand (lookAhead stop) >> (return $ reverse revList)) <|>
       (assignment expand >> untilRec revList) <|>
       (expand (categoryToken BeginGroup) >> beginGroup >> untilRec revList) <|>
       (expand (categoryToken EndGroup) >> endGroup >> untilRec revList) <|>
       (expand anyToken >>= (\tok -> untilRec (tok:revList))))

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
