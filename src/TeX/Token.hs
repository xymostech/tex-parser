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

instance Ord Token where
  (<=) (CharToken c cat) (CharToken c' cat') = (c, cat) <= (c', cat')
  (<=) (ControlSequence cs) (ControlSequence cs') = cs <= cs'
  (<=) (CharToken _ _) (ControlSequence _) = True
  (<=) (ControlSequence _) (CharToken _ _) = False

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
