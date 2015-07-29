module Main where

import Prelude (Maybe(Just, Nothing), IO, return)
import TeX.Lexer
import TeX.Category
import Data.Map()
import Data.List()

lexAll :: Lexer -> CategoryMap -> [LexToken]
lexAll lexer map =
  case lexToken lexer map of
    Just (token, newLexer) -> token:(lexAll newLexer map)
    Nothing -> []

main :: IO ()
main =
  return ()
