module Main where

import Prelude (Char, Maybe(Just, Nothing), IO, return)
import Test.HUnit

import TeX.Category
import TeX.Lexer

myLexerMap :: CategoryMap
myLexerMap = set '^' Superscript initialMap

assertLexesTo :: [[Char]] -> [LexToken] -> Assertion
assertLexesTo lines tokens =
  assertEqual "" (lexAll (mkLexer lines) myLexerMap) tokens

lexAll :: Lexer -> CategoryMap -> [LexToken]
lexAll lexer map =
  case lexToken lexer map of
    Just (token, newLexer) -> token:(lexAll newLexer map)
    Nothing -> []

lexerTests :: Test
lexerTests =
  test
  [ "trigraphs" ~: assertLexesTo ["^^:%"] [Token 'z' Letter]
  , "recursive trigraphs" ~: assertLexesTo ["^^\RS^:%"] [Token 'z' Letter]
  , "hex trigraphs" ~: assertLexesTo ["^^7a%"] [Token 'z' Letter]
  , "not hex trigraphs" ~: assertLexesTo ["^^7g%"] [Token 'w' Letter, Token 'g' Letter]
  , "trigraphs in controlseqs" ~: assertLexesTo ["^^\FSab \\a^^:%"] [ControlSequence "ab", ControlSequence "az"]
  , "multiple tokens" ~: assertLexesTo ["ab%"] [Token 'a' Letter, Token 'b' Letter]
  , "ignores leading spaces" ~: assertLexesTo [" a%"] [Token 'a' Letter]
  , "ignores trailing spaces" ~: assertLexesTo ["a "] [Token 'a' Letter, Token ' ' Space]
  , "ignores text after comments" ~: assertLexesTo ["a%b"] [Token 'a' Letter]
  , "single letter controlseqs" ~: assertLexesTo ["\\a1%"] [ControlSequence "a", Token '1' Other]
  , "single other controlseqs" ~: assertLexesTo ["\\&a%"] [ControlSequence "&", Token 'a' Letter]
  , "multi letter controlseqs" ~: assertLexesTo ["\\abc1%"] [ControlSequence "abc", Token '1' Other]
  , "ignores spaces after controlseqs" ~: assertLexesTo ["\\a \\abc \\  %"]
                                                        [ControlSequence "a", ControlSequence "abc", ControlSequence " "]
  , "keeps spaces after controlseqs" ~: assertLexesTo ["\\& a%"] [ControlSequence "&", Token ' ' Space, Token 'a' Letter]
  , "newlines to pars" ~: assertLexesTo ["a%", "", "a%"] [Token 'a' Letter, ControlSequence "par", Token 'a' Letter]
  , "ignored" ~: assertLexesTo ["a\NULb%"] [Token 'a' Letter, Token 'b' Letter]
  , "ignored spaces" ~: assertLexesTo [" a  ", " a%"] [Token 'a' Letter, Token ' ' Space, Token 'a' Letter]
  ]

main :: IO ()
main = do
  _ <- runTestTT lexerTests
  return ()
