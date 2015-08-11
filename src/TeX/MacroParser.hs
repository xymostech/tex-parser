module TeX.MacroParser where

import Text.Parsec ( try, option, choice, lookAhead, anyToken
                   , (<|>), (<?>)
                   )
import Data.Char (ord)
import qualified Data.Map as M
import Control.Monad.State (lift, modify, gets)

import TeX.Parser
import TeX.Token
import TeX.Category
import TeX.Def

toInt :: Char -> Int
toInt c = (ord c) - (ord '0')

numberedParameter :: TeXParser Int
numberedParameter = do
  _ <- categoryToken Parameter
  num <- choice
         [ exactToken $ CharToken num Other
             | num <- ['0'..'9']]
  return $ toInt $ extractChar num

trailingBrace :: TeXParser ParameterToken
trailingBrace = do
  _ <- categoryToken Parameter
  _ <- lookAhead $ categoryToken BeginGroup
  return $ PTTrailingBrace

allowedTokens :: TeXParser Token
allowedTokens =
  choice [ categoryToken cat
             | cat <- [ MathShift, AlignmentTab, Superscript
                      , Subscript, Space, Letter, Other
                      , Active]]

parseParameterToken :: TeXParser ParameterToken
parseParameterToken = do
  (PTToken <$> allowedTokens) <|>
   (PTToken <$> controlSequence) <|>
   try (PTParameter <$> numberedParameter) <|>
   trailingBrace <?> "parameter list token"

parseParameterText :: Int -> TeXParser [ParameterToken]
parseParameterText num = do
  token <- parseParameterToken
  nextNum <- case token of
    PTParameter n -> if n == num
                         then return (num + 1)
                         else fail "macro parameters in wrong order"
    _ -> return num
  rest <- option [] $ parseParameterText nextNum
  return $ (token:rest)

parseBalancedTextRec :: Int -> TeXParser [Token]
parseBalancedTextRec level = do
  (token, newLevel) <-
    openBrace <|>
    closeBrace <|>
    ((,) <$> (categoryToken Parameter) <*> (return level)) <|>
    ((,) <$> (controlSequence) <*> (return level)) <|>
    ((,) <$> (allowedTokens) <*> (return level)) <?>
    "balanced text token"
  rest <- option [] $ (levelZero newLevel) <|> parseBalancedTextRec newLevel
  return $ (token:rest)
  where
    openBrace = do
      tok <- categoryToken BeginGroup
      return (tok, level + 1)
    closeBrace = do
      tok <- categoryToken EndGroup
      return (tok, level - 1)
    levelZero l
      | l == 0 = lookAhead $ categoryToken EndGroup >> return []
      | otherwise = fail "not at level 0"

parseBalancedText :: TeXParser [Token]
parseBalancedText = do
  (lookAhead (categoryToken EndGroup) >> return []) <|>
   parseBalancedTextRec 0

-- TODO(emily): make this error if there's a parameter greater than the number allowed
makeReplacementText :: [Token] -> TeXParser [ReplacementToken]
makeReplacementText ((CharToken _ Parameter):t@(CharToken _ Parameter):rest) =
  (:) (RTToken t) <$> makeReplacementText rest
makeReplacementText ((CharToken _ Parameter):(CharToken n Other):rest)
  | n >= '0' && n <= '9' = (:) (RTParameter $ toInt n) <$> makeReplacementText rest
  | otherwise = fail "invalid replacement token"
makeReplacementText ((CharToken _ Parameter):_) =
  fail "invalid replacement token"
makeReplacementText (t:rest) =
  (:) (RTToken t) <$> makeReplacementText rest
makeReplacementText [] = return []

parseDef :: TeXParser Def
parseDef = do
  _ <- exactToken (ControlSequence "def")
  control <- controlSequence
  parameterText <- option [] $ try (parseParameterText 1)
  _ <- categoryToken BeginGroup
  replacementText <- try (parseBalancedText >>= makeReplacementText)
  _ <- categoryToken EndGroup
  return $ Def (extractControlSequence control) parameterText replacementText

parseParams :: [ParameterToken] -> TeXParser [[Token]]
parseParams [] = return []
parseParams ((PTToken token):ps) = do
  token' <- anyToken
  if token == token'
  then parseParams ps
  else fail $ "mismatched token: " ++ (show token)
parseParams ((PTParameter _):ps) = do
  tokens <- case ps of
    [] -> undelimitedParameter
    ((PTParameter _):_) -> undelimitedParameter
    _ -> delimitedParameter
  restTokens <- parseParams ps
  return (tokens:restTokens)
  where
    undelimitedParameter = do
      (try $ do
        _ <- categoryToken BeginGroup
        text <- try parseBalancedText
        _ <- categoryToken EndGroup
        return text) <|>
       (categoryToken Space >> undelimitedParameter) <|>
             -- anyToken probably shouldn't include #?
       (:) <$> anyToken <*> (return [])

    tryMatch :: [ParameterToken] -> TeXParser ()
    tryMatch [] = return ()
    tryMatch ((PTParameter _):_) = return ()
    tryMatch (PTTrailingBrace:_) = return ()
    tryMatch ((PTToken tok):rest) = do
      tok' <- anyToken
      if tok == tok'
      then tryMatch rest
      else fail "mismatched token"

    tryDelimiter :: [Token] -> TeXParser [Token]
    tryDelimiter currToks = do
      (lookAhead (try (tryMatch ps)) >> return currToks) <|>
       (try $ do
          left <- categoryToken BeginGroup
          text <- try parseBalancedText
          right <- categoryToken EndGroup
          case currToks of
            [] -> (lookAhead (try (tryMatch ps)) >> return text) <|>
                   (tryDelimiter $ [left] ++ text ++ [right])
            _ -> tryDelimiter $ currToks ++ [left] ++ text ++ [right]
       ) <|>
       (do
         newTok <- anyToken
         tryDelimiter (currToks ++ [newTok]))

    delimitedParameter = tryDelimiter []
parseParams (PTTrailingBrace:_) = fail "unimplemented pttrailingbrace"

replaceParams :: [ReplacementToken] -> [[Token]] -> [Token]
replaceParams [] _ = []
replaceParams ((RTToken tok):rest) args = tok:(replaceParams rest args)
replaceParams ((RTParameter num):rest) args =
  (args !! (num - 1)) ++ replaceParams rest args

parseExpansion :: Def -> TeXParser [Token]
parseExpansion (Def func params replacement) = do
  _ <- exactToken (ControlSequence func)
  replacementParams <- parseParams params
  return $ replaceParams replacement replacementParams

runExpansion :: Def -> TeXParser ()
runExpansion def = do
  tokens <- parseExpansion def
  prependTokens tokens

parseMacros :: TeXParser ()
parseMacros = do
  tok <- lookAhead controlSequence
  case extractControlSequence tok of
    "def" -> do
      def@(Def name _ _) <- parseDef
      lift $ modify (\st -> st { stateDefinitionMap = M.insert name def $ stateDefinitionMap st })
      return ()
    name -> do
      defMap <- lift $ gets stateDefinitionMap
      case M.lookup name defMap of
        Just def -> runExpansion def
        Nothing -> fail "unknown macro"
