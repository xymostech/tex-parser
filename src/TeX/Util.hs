module TeX.Util
( lexAll
)
where

import Prelude (Maybe(Just, Nothing))

import TeX.Lexer
import TeX.State
import TeX.Token

lexAll :: Lexer -> TeXState -> [Token]
lexAll lexer texState =
  case lexToken lexer texState of
    Just (token, newLexer) -> token:(lexAll newLexer texState)
    Nothing -> []
