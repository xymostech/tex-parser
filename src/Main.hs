{-# OPTIONS_GHC -fno-warn-unused-imports #-}
module Main where

import Prelude (Maybe(Just, Nothing), IO, Char, Either, return, ($))
import qualified Data.Map as M
import qualified Data.List as L
import Text.Parsec
import Control.Lens
import Control.Monad.State as S

import TeX.Lexer
import TeX.Category
import TeX.Parser.Parser
import TeX.Parser.HorizontalList
import TeX.Def
import TeX.Parser.MacroParser
import TeX.Token
import TeX.State

defaultMap :: CategoryMap
defaultMap =
  (category '{' .~ Just BeginGroup) $
  (category '}' .~ Just EndGroup) $
  (category '#' .~ Just Parameter) $
  (category '^' .~ Just Superscript) initialMap

tryParser :: TeXParser a -> [Char] -> Either ParseError a
tryParser parser str =
  TeX.Parser.runParser parser (Just $ mkState defaultMap) lines
  where
    lines = [str]

main :: IO ()
main =
  return ()
