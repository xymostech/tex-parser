{-# LANGUAGE TypeSynonymInstances,
             FlexibleInstances,
             RankNTypes,
             MultiParamTypeClasses #-}
module TeX.Parser.Parser where

import Prelude ( Maybe(Just, Nothing), Show, Char, Either()
               , return, id
               , ($), (++)
               )
import Control.Monad.Identity (Identity, runIdentity)
import Text.Parsec ( ParsecT, Stream(uncons), ParseError
                   , runParserT
                   , getInput, setInput
                   , getState, setState
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
  , streamState :: TeXState
  }
  deriving (Show)

mkStream :: TeXState -> [[Char]] -> TeXLexerStream
mkStream state lines =
  TeXLexerStream
  { streamLexer = mkLexer lines
  , streamNextTokens = []
  , streamState = state
  }

instance Stream TeXLexerStream Identity Token where
  -- uncons :: TeXLexerStream -> S.State TeXState (Maybe (Token, TeXLexerStream))
  uncons (TeXLexerStream lexer [] state) = do
    return $ case lexToken lexer state of
               Just (lexedToken, newLexer) ->
                 Just (lexedToken, TeXLexerStream newLexer [] state)
               Nothing -> Nothing
  uncons (TeXLexerStream lexer (tok:toks) state) = do
    return $ Just (tok, TeXLexerStream lexer toks state)

type TeXParser = ParsecT TeXLexerStream TeXState Identity

prependTokens :: [Token] -> TeXParser ()
prependTokens newToks = do
  (TeXLexerStream lexer toks catMap) <- getInput
  setInput $ (TeXLexerStream lexer (newToks ++ toks) catMap)

runParser :: TeXParser a -> Maybe TeXState -> [[Char]] -> Either ParseError a
runParser parser maybeState lines =
  runIdentity toParse
  where
    toParse =
      runParserT parser state "main.tex" (mkStream state lines)

    state = case maybeState of
              Just s -> s
              Nothing -> mkState initialMap

type Expander = forall a. TeXParser a -> TeXParser a

noExpand :: Expander
noExpand = id

beginGroup :: TeXParser ()
beginGroup = do
  state <- getState
  setState $ pushState state

endGroup :: TeXParser ()
endGroup = do
  state <- getState
  setState $ popState state
