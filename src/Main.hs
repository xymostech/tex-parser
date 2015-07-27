module Main where

import Prelude hiding (lex)
import qualified Data.List as L

import Control.Monad.Trans.Maybe
import Control.Monad.State.Lazy
import Control.Monad.Trans.Class
import Debug.Trace

type Lexer a = State [Char] a
type MaybeLexer a = MaybeT (State [Char]) a

isEOF :: MaybeLexer Bool
isEOF = do
  eof <- gets (\x -> length x == 0)
  return eof

lexChar :: MaybeLexer Char
lexChar = do
  True <- isEOF
  char <- lift $ gets (\(x:_) -> x)
  lift $ modify (\(_:xs) -> xs)
  return char

contains :: (Eq a) => [a] -> a -> Bool
contains xs x = any (==x) xs

letters = ['a'..'z'] ++ ['A'..'Z']

lexN :: Int -> MaybeLexer [Char]
lexN 0 = return []
lexN n = do
  char <- lexChar
  nextChars <- traceShow char $ lexN (n - 1)
  return []

{--
lexWord :: Lexer String
lexWord = do
  return ""

lexWords :: Lexer [String]
lexWords = do
  return []

lowercase = ['a'..'z']

lexLowercase :: Lexer [Char]
lexLowercase = do
  eof <- isEOF
  if eof
  then return []
  else do
    char <- lexChar
    rest <- lexLowercase
    return $ if any (==char) lowercase
             then (char:rest)
             else rest

--}

main :: IO ()
main = do
  putStrLn "blah"

  print $ runState (do
         Just x <- runMaybeT $ lexN 3
         return x) "blah"

