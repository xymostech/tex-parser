{-# OPTIONS_GHC -fno-warn-unused-imports #-}
module Main where

import Prelude
import qualified Data.Map as M
import qualified Data.List as L
import Text.Parsec hiding (runParser)
import Control.Lens
import Control.Monad.State as S
import Control.Applicative
import System.Exit (die)

import TeX.Category
import TeX.Count
import TeX.Def
import TeX.Iterator
import TeX.Lexer
{--
import TeX.Parser.Expand
import TeX.Parser.Assignment
import TeX.Parser.HorizontalList
import TeX.Parser.MacroParser
import TeX.Parser.Parser
import TeX.Parser.Prim
import TeX.Parser.Conditional
import TeX.Parser.Util
import TeX.State
import TeX.Token

defaultMap :: CategoryMap
defaultMap =
  (category '{' .~ Just BeginGroup) $
  (category '}' .~ Just EndGroup) $
  (category '#' .~ Just Parameter) $
  (category '^' .~ Just Superscript) initialMap

tryParser :: TeXParser a -> [Char] -> Either ParseError a
tryParser parser str =
  TeX.Parser.Parser.runParser parser (Just $ mkState defaultMap) [str]

main :: IO ()
main = do
  input <- getContents
  let inputLines = splitInput input
  case runParser (horizontalListChars <* expand eof) (Just $ mkState defaultMap) inputLines of
    Left err ->
      die $ show err
    Right output ->
      putStrLn output
--}

main = return ()
