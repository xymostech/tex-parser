{-# LANGUAGE TypeSynonymInstances,
             FlexibleInstances,
             MultiParamTypeClasses #-}
module TeX.Parser where

import Prelude ( Maybe(Just, Nothing), Int, Show
               , show, otherwise
               , ($), (==), (<*), (*>), (++)
               )
import Control.Monad.State as S
import Text.Parsec ( ParsecT, Stream(uncons)
                   , tokenPrim, try, anyToken, lookAhead
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
  { categoryMap :: CategoryMap
  , definitionMap :: DefinitionMap
  }
  deriving (Show)

mkState :: CategoryMap -> TeXState
mkState catMap = TeXState catMap emptyDefMap

data TeXLexerStream = TeXLexerStream Lexer [Token]
  deriving (Show)

instance Stream TeXLexerStream (S.State TeXState) Token where
  -- uncons :: TeXLexerStream -> S.State TeXState (Maybe (Token, TeXLexerStream))
  uncons st@(TeXLexerStream lexer []) = do
    catMap <- gets categoryMap
    return $ case lexToken lexer catMap of
               Just (lexedToken, newLexer) ->
                 Just (lexedToken, TeXLexerStream newLexer [])
               Nothing -> Nothing
  uncons st@(TeXLexerStream lexer (tok:toks)) = do
    return $ Just (tok, TeXLexerStream lexer toks)

data TeXParserState = TeXParserState Int

type TeXParser = ParsecT TeXLexerStream TeXParserState (S.State TeXState)

prependTokens :: [Token] -> TeXParser ()
prependTokens newToks = do
  (TeXLexerStream lexer toks) <- getInput
  setInput $ (TeXLexerStream lexer (newToks ++ toks))

runGrouped :: TeXParser a -> TeXParser a
runGrouped p = do
  oldState <- lift $ get
  statefulTry (p <* put oldState)

statefulTry :: TeXParser a -> TeXParser a
statefulTry p = do
  oldState <- lift $ get
  try p <|> do
    lift $ put oldState
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
