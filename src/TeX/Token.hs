module TeX.Token
( Token(CharToken, ControlSequence)
, extractChar, extractControlSequence
)
where

import TeX.Category

data Token = CharToken Char Category
           | ControlSequence [Char]
  deriving (Show, Eq)

extractChar :: Token -> Char
extractChar (CharToken c _) = c
extractChar _ = error "Can't extract character from control sequence"

extractControlSequence :: Token -> [Char]
extractControlSequence (ControlSequence s) = s
extractControlSequence _ = error "Can't extract control sequence from token"
