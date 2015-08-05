module Main where

import Prelude (Char, Maybe(Just, Nothing), IO, Either(Left, Right), Eq, Show
               , return, fst
               , ($), (<*)
               )
import Text.Parsec (ParseError, runParserT, eof)
import Control.Monad.State (runState)
import Test.HUnit

import TeX.Category
import TeX.Def
import TeX.HorizontalList
import TeX.Lexer
import TeX.MacroParser
import TeX.Parser
import TeX.Token

myLexerMap :: CategoryMap
myLexerMap = set '^' Superscript initialMap

assertLexesTo :: [[Char]] -> [Token] -> Assertion
assertLexesTo lines tokens =
  assertEqual "" (lexAll (mkLexer lines) myLexerMap) tokens

lexAll :: Lexer -> CategoryMap -> [Token]
lexAll lexer map =
  case lexToken lexer map of
    Just (token, newLexer) -> token:(lexAll newLexer map)
    Nothing -> []

lexerTests :: Test
lexerTests =
  test
  [ "trigraphs" ~: assertLexesTo ["^^:%"] [CharToken 'z' Letter]
  , "recursive trigraphs" ~: assertLexesTo ["^^\RS^:%"] [CharToken 'z' Letter]
  , "hex trigraphs" ~: assertLexesTo ["^^7a%"] [CharToken 'z' Letter]
  , "not hex trigraphs" ~: assertLexesTo ["^^7g%"] [CharToken 'w' Letter, CharToken 'g' Letter]
  , "trigraphs in controlseqs" ~: assertLexesTo ["^^\FSab \\a^^:%"] [ControlSequence "ab", ControlSequence "az"]
  , "multiple tokens" ~: assertLexesTo ["ab%"] [CharToken 'a' Letter, CharToken 'b' Letter]
  , "ignores leading spaces" ~: assertLexesTo [" a%"] [CharToken 'a' Letter]
  , "ignores trailing spaces" ~: assertLexesTo ["a "] [CharToken 'a' Letter, CharToken ' ' Space]
  , "ignores text after comments" ~: assertLexesTo ["a%b"] [CharToken 'a' Letter]
  , "single letter controlseqs" ~: assertLexesTo ["\\a1%"] [ControlSequence "a", CharToken '1' Other]
  , "single other controlseqs" ~: assertLexesTo ["\\&a%"] [ControlSequence "&", CharToken 'a' Letter]
  , "multi letter controlseqs" ~: assertLexesTo ["\\abc1%"] [ControlSequence "abc", CharToken '1' Other]
  , "ignores spaces after controlseqs" ~: assertLexesTo ["\\a \\abc \\  %"]
                                                        [ControlSequence "a", ControlSequence "abc", ControlSequence " "]
  , "keeps spaces after controlseqs" ~: assertLexesTo ["\\& a%"] [ControlSequence "&", CharToken ' ' Space, CharToken 'a' Letter]
  , "newlines to pars" ~: assertLexesTo ["a%", "", "a%"] [CharToken 'a' Letter, ControlSequence "par", CharToken 'a' Letter]
  , "ignored" ~: assertLexesTo ["a\NULb%"] [CharToken 'a' Letter, CharToken 'b' Letter]
  , "ignored spaces" ~: assertLexesTo [" a  ", " a%"] [CharToken 'a' Letter, CharToken ' ' Space, CharToken 'a' Letter]
  ]

myParserMap = set '#' Parameter $ set '{' BeginGroup $ set '}' EndGroup $ initialMap

doParse :: TeXParser a -> [[Char]] -> Either ParseError a
doParse parser lines =
  fst $ runState testParse $ mkState myParserMap
  where
    testParse =
      runParserT parser
                   ()
                   "main.tex"
                   (TeXLexerStream (mkLexer lines) [])

assertParsesTo :: (Eq a, Show a) => TeXParser a -> [[Char]] -> a -> Assertion
assertParsesTo p lines expected =
  assertEqual "" (Right expected) (doParse (p <* eof) lines)

assertExpandsTo :: Def -> [[Char]] -> [Token] -> Assertion
assertExpandsTo def lines expected =
  assertEqual "" (Right expected) (doParse (parseExpansion def <* eof) lines)

macroTests =
  test
  [ "parses empty macros" ~: assertParsesTo parseDef ["\\def\\a{}%"] (Def "a" [] [])
  , "parses basic macros" ~: assertParsesTo parseDef ["\\def\\a{b}%"] (Def "a" [] [RTToken (CharToken 'b' Letter)])
  ]

parserTests =
  test
  [ "basic objects" ~: assertParsesTo horizontalList ["a+%"] [HBoxChar 'a', HBoxChar '+']
  -- TODO(emily): this should actually be a glue
  , "spaces" ~: assertParsesTo horizontalList ["a %"] [HBoxChar 'a', HBoxChar ' ']
  ]

main :: IO ()
main = do
  _ <- runTestTT $ "lexer" ~: lexerTests
  _ <- runTestTT $ "macros" ~: macroTests
  _ <- runTestTT $ "parser" ~: parserTests
  return ()
