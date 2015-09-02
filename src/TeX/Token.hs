module TeX.Token
( Token(CharToken, ControlSequence)
, extractChar, extractControlSequence, charCode
)
where

import Data.Char (ord)

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

charCode :: Token -> Maybe Integer
charCode (CharToken c _)
  | c >= '0' && c <= '9' = Just $ fromIntegral $ (ord c) - (ord '0')
  | c >= 'A' && c <= 'F' = Just $ fromIntegral $ (ord c) - (ord 'A') + 10
  | otherwise = Nothing
charCode _ = Nothing
