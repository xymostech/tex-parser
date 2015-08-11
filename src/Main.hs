{-# OPTIONS_GHC -fno-warn-unused-imports #-}
module Main where

import Prelude (Maybe(Just, Nothing), IO, Char, Either, return, ($))
import qualified Data.Map as M
import qualified Data.List as L
import Text.Parsec
import Control.Monad.State as S

import TeX.Lexer
import TeX.Category
import TeX.Parser
import TeX.HorizontalList
import TeX.Def
import TeX.MacroParser
import TeX.Token

lexAll :: Lexer -> CategoryMap -> [Token]
lexAll lexer map =
  case lexToken lexer map of
    Just (tok, newLexer) -> tok:(lexAll newLexer map)
    Nothing -> []

defaultMap :: CategoryMap
defaultMap = set '{' BeginGroup $ set '}' EndGroup $ set '#' Parameter $ set '^' Superscript initialMap

tryParser :: TeXParser a -> [Char] -> Either ParseError a
tryParser parser str =
  TeX.Parser.runParser parser (Just $ mkState defaultMap) lines
  where
    lines = [str]

main :: IO ()
main =
  return ()
