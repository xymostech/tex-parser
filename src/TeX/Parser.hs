{-# LANGUAGE TypeSynonymInstances,
             FlexibleInstances,
             MultiParamTypeClasses #-}
module TeX.Parser where

import Prelude ( Maybe(Just, Nothing), Show, Char, Either()
               , show, otherwise, return
               , ($), (==), (<*), (++)
               )
import Control.Monad.Identity (Identity, runIdentity)
import Text.Parsec ( ParsecT, Stream(uncons), ParseError
                   , tokenPrim, runParserT
                   , getInput, setInput, getState, putState
                   )
import Debug.Trace

import TeX.Lexer
import TeX.Category
import TeX.Token
import TeX.State

myTrace :: (Show a, Show b) => b -> a -> a
myTrace str a = traceShow (str, a) a

data TeXLexerStream = TeXLexerStream
  { streamLexer :: Lexer
  , streamNextTokens :: [Token]
  , streamCategoryMap :: CategoryMap
  }
  deriving (Show)

mkStream :: TeXState -> [[Char]] -> TeXLexerStream
mkStream state lines =
  TeXLexerStream
  { streamLexer = mkLexer lines
  , streamNextTokens = []
  , streamCategoryMap = stateCategoryMap state
  }

instance Stream TeXLexerStream Identity Token where
  -- uncons :: TeXLexerStream -> S.State TeXState (Maybe (Token, TeXLexerStream))
  uncons (TeXLexerStream lexer [] catMap) = do
    return $ case lexToken lexer catMap of
               Just (lexedToken, newLexer) ->
                 Just (lexedToken, TeXLexerStream newLexer [] catMap)
               Nothing -> Nothing
  uncons (TeXLexerStream lexer (tok:toks) catMap) = do
    return $ Just (tok, TeXLexerStream lexer toks catMap)

type TeXParser = ParsecT TeXLexerStream TeXState Identity

prependTokens :: [Token] -> TeXParser ()
prependTokens newToks = do
  (TeXLexerStream lexer toks catMap) <- getInput
  setInput $ (TeXLexerStream lexer (newToks ++ toks) catMap)

runGrouped :: TeXParser a -> TeXParser a
runGrouped p = do
  oldState <- getState
  p <* (putState oldState)

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
  runIdentity toParse
  where
    toParse =
      runParserT parser state "main.tex" (mkStream state lines)

    state = case maybeState of
              Just s -> s
              Nothing -> mkState initialMap
