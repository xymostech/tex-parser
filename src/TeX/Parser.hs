{-# LANGUAGE TypeSynonymInstances,
             FlexibleInstances,
             MultiParamTypeClasses #-}
module TeX.Parser where

import Prelude ( Maybe(Just, Nothing), Show, Char, Either()
               , show, otherwise, fst, return, fail
               , ($), (==), (<*), (++)
               )
import qualified Control.Monad.State as S
import Text.Parsec ( ParsecT, Stream(uncons), ParseError
                   , tokenPrim, try, runParserT
                   , getInput, setInput
                   , (<|>))
import Debug.Trace

import TeX.Lexer
import TeX.Category
import TeX.Token
import TeX.Def

myTrace :: (Show a, Show b) => b -> a -> a
myTrace str a = traceShow (str, a) a

data TeXState = TeXState
  { stateCategoryMap :: CategoryMap
  , stateDefinitionMap :: DefinitionMap
  }
  deriving (Show)

mkState :: CategoryMap -> TeXState
mkState catMap = TeXState catMap emptyDefMap

data TeXLexerStream = TeXLexerStream
  { streamLexer :: Lexer
  , streamNextTokens :: [Token]
  , streamCategoryMap :: CategoryMap
  }
  deriving (Show)

mkStream :: CategoryMap -> [[Char]] -> TeXLexerStream
mkStream map lines =
  TeXLexerStream
  { streamLexer = mkLexer lines
  , streamNextTokens = []
  , streamCategoryMap = map
  }

instance Stream TeXLexerStream (S.State TeXState) Token where
  -- uncons :: TeXLexerStream -> S.State TeXState (Maybe (Token, TeXLexerStream))
  uncons (TeXLexerStream lexer [] _) = do
    catMap <- S.gets stateCategoryMap
    return $ case lexToken lexer catMap of
               Just (lexedToken, newLexer) ->
                 Just (lexedToken, TeXLexerStream newLexer [] catMap)
               Nothing -> Nothing
  uncons (TeXLexerStream lexer (tok:toks) catMap) = do
    return $ Just (tok, TeXLexerStream lexer toks catMap)

type TeXParser = ParsecT TeXLexerStream () (S.State TeXState)

prependTokens :: [Token] -> TeXParser ()
prependTokens newToks = do
  (TeXLexerStream lexer toks catMap) <- getInput
  setInput $ (TeXLexerStream lexer (newToks ++ toks) catMap)

runGrouped :: TeXParser a -> TeXParser a
runGrouped p = do
  oldState <- S.lift $ S.get
  statefulTry (p <* S.put oldState)

statefulTry :: TeXParser a -> TeXParser a
statefulTry p = do
  oldState <- S.lift $ S.get
  try p <|> do
    S.lift $ S.put oldState
    fail "restoring state"

tokenWithFunc :: (Token -> Maybe Token) -> TeXParser Token
tokenWithFunc func =
  tokenPrim showToken nextPos func
  where
    showToken = show
    nextPos pos _ _ = pos

controlSequence :: TeXParser Token
controlSequence =
  tokenWithFunc testControlSequence
  where
    testControlSequence esc@(ControlSequence _) = Just esc
    testControlSequence _ = Nothing

exactToken :: Token -> TeXParser Token
exactToken tok =
  tokenWithFunc testToken
  where
    testToken tok'
      | tok == tok' = Just tok
      | otherwise = Nothing

categoryToken :: Category -> TeXParser Token
categoryToken cat =
  tokenWithFunc testToken
  where
    testToken tok@(CharToken _ cat')
      | cat == cat' = Just tok
      | otherwise = Nothing
    testToken _ = Nothing

runParser :: TeXParser a -> Maybe TeXState -> [[Char]] -> Either ParseError a
runParser parser maybeState lines =
  fst $ S.runState toParse state
  where
    -- toParse :: (S.State TeXState) (Either ParseError a)
    toParse =
      runParserT parser () "main.tex" (mkStream (stateCategoryMap state) lines)

    state = case maybeState of
              Just s -> s
              Nothing -> mkState initialMap
