module Main where

import Data.Either (Either(Right), isLeft)
import Prelude (Char, Maybe(Just), IO, Eq, Show, String, Bool
               , return, putStrLn, sequence, all, id
               , ($), (<*), (+), (==)
               )
import System.Exit (exitSuccess, exitFailure)
import Test.HUnit ( Assertion, Test
                  , assertEqual, assertBool, test, runTestTT, failures, errors
                  , (~:)
                  )
import Text.Parsec (ParseError, eof)
import Control.Lens ((.~))

import TeX.Category
import TeX.Def
import TeX.HorizontalList
import TeX.Lexer
import TeX.MacroParser
import TeX.Parser
import TeX.Token
import TeX.State
import TeX.Util

myLexerMap :: CategoryMap
myLexerMap = (category '^' .~ Just Superscript) initialMap

assertLexesTo :: [[Char]] -> [Token] -> Assertion
assertLexesTo lines tokens =
  assertEqual "" (lexAll (mkLexer lines) (mkState myLexerMap)) tokens

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

myParserMap :: CategoryMap
myParserMap = (category '#' .~ Just Parameter) $
              (category '{' .~ Just BeginGroup) $
              (category '}' .~ Just EndGroup) $ initialMap

doParse :: TeXParser a -> [[Char]] -> Either ParseError a
doParse parser lines =
  runParser parser (Just $ mkState myParserMap) lines

assertDoesntParse :: TeXParser a -> [[Char]] -> Assertion
assertDoesntParse p lines =
  assertBool "parse failed" (isLeft $ doParse (p <* eof) lines)

assertParsesTo :: (Eq a, Show a) => TeXParser a -> [[Char]] -> a -> Assertion
assertParsesTo p lines expected =
  assertEqual "" (Right expected) (doParse (p <* eof) lines)

assertExpandsTo :: Def -> [[Char]] -> [Token] -> Assertion
assertExpandsTo def lines expected =
  assertEqual "" (Right expected) (doParse (parseExpansion def <* eof) lines)

assertDoesntExpand :: Def -> [[Char]] -> Assertion
assertDoesntExpand def lines =
  assertBool "parse failed" (isLeft $ doParse (parseExpansion def <* eof) lines)

macroTests :: Test
macroTests =
  test
  [ "parses empty macros" ~: assertParsesTo parseDef ["\\def\\a{}%"] (Def "a" [] [])
  , "parses basic macros" ~: assertParsesTo parseDef ["\\def\\a{b}%"] (Def "a" [] [RTToken (CharToken 'b' Letter)])
  , "parses macros with arguments" ~: assertParsesTo parseDef ["\\def\\a#1{}%"] (Def "a" [PTParameter 1] [])
  , "parses macros with multiple arguments" ~: assertParsesTo parseDef ["\\def\\a#1#2#3{}%"] (Def "a" [PTParameter 1, PTParameter 2, PTParameter 3] [])
  , "doesn't parse macros with backwards args" ~: assertDoesntParse parseDef ["\\def\\a#2#1{}%"]
  , "doesn't parse macros with out-of-order args" ~: assertDoesntParse parseDef ["\\def\\a#1#3{}%"]
  , "doesn't parse macros with duplicate args" ~: assertDoesntParse parseDef ["\\def\\a#1#1{}%"]
  , "parses macros with args in their bodies" ~: assertParsesTo parseDef ["\\def\\a#1{#1}%"] (Def "a" [PTParameter 1] [RTParameter 1])
  , "parses macros with multiple args in their bodies" ~: assertParsesTo parseDef ["\\def\\a#1#2{#2#1#1#2}%"] (Def "a" [PTParameter 1, PTParameter 2] [RTParameter 2, RTParameter 1, RTParameter 1, RTParameter 2])
  , "parses macros with balanced bodies" ~: assertParsesTo parseDef ["\\def\\a{{}{{}}}%"] (Def "a" [] [openBraceRT, closeBraceRT, openBraceRT, openBraceRT, closeBraceRT, closeBraceRT])
  , "doesn't parse macros with imbalanced bodies" ~: assertDoesntParse parseDef ["\\def\\a{{}{{}}%"]
  , "parses double pound signs" ~: assertParsesTo parseDef ["\\def\\a{##}%"] (Def "a" [] [RTToken (CharToken '#' Parameter)])
  , "parses other tokens in the replacement text" ~: assertParsesTo parseDef ["\\def\\a{a#1 \\x##}%"] (Def "a" [] [RTToken (CharToken 'a' Letter), RTParameter 1, RTToken (CharToken ' ' Space), RTToken (ControlSequence "x"), RTToken (CharToken '#' Parameter)])
  , "parses macros with newlines" ~: assertParsesTo parseDef ["\\def\\a{", "}%"] (Def "a" [] [RTToken (CharToken ' ' Space)])
  , "parses macros with tokens in the parameters" ~: assertParsesTo parseDef ["\\def\\a 1#12{}%"] (Def "a" [PTToken (CharToken '1' Other), PTParameter 1, PTToken (CharToken '2' Other)] [])
  , "parses trailing parameter braces" ~: assertParsesTo parseDef ["\\def\\a #1#{}%"] (Def "a" [PTParameter 1, PTTrailingBrace] [])
  , "doesn't parse parameters in the replacement text" ~: assertDoesntParse parseDef ["\\def\\a{#}%"]
  -- \def\a{} \a -> ''
  , "expands empty macros" ~: assertExpandsTo (Def "a" [] []) ["\\a%"] []
  -- \def\a{ab} \a -> 'ab'
  , "expands no-arg macros" ~: assertExpandsTo (Def "a" [] [RTToken (CharToken 'a' Letter), RTToken (CharToken 'b' Letter)]) ["\\a%"] [CharToken 'a' Letter, CharToken 'b' Letter]
  -- \def\a#1{a#1b} \a{} -> 'ab'
  , "expands empty groups" ~: assertExpandsTo (Def "a" [PTParameter 1] [RTToken (CharToken 'a' Letter), RTParameter 1, RTToken (CharToken 'b' Letter)]) ["\\a{}%"] [CharToken 'a' Letter, CharToken 'b' Letter]
  -- \def\a#1{#1} \a1 -> '1'
  , "expands single argument macros with single token arg" ~: assertExpandsTo (Def "a" [PTParameter 1] [RTParameter 1]) ["\\a1%"] [CharToken '1' Other]
  -- \def\a#1{#1} \a{ab} -> 'ab'
  , "expands single argument macros with a group arg" ~: assertExpandsTo (Def "a" [PTParameter 1] [RTParameter 1]) ["\\a{ab}%"] [CharToken 'a' Letter, CharToken 'b' Letter]
  -- \def\a#1#2{#1#2} \a {12}3 -> '123'
  , "expands multiple argument macros" ~: assertExpandsTo (Def "a" [PTParameter 1, PTParameter 2] [RTParameter 1, RTParameter 2]) ["\\a {12}3%"] [CharToken '1' Other, CharToken '2' Other, CharToken '3' Other]
  -- \def\a#1#2{#1#2} \a {12}  3 -> '123'
  , "expands multiple arg macros ignoring spaces" ~: assertExpandsTo (Def "a" [PTParameter 1, PTParameter 2] [RTParameter 1, RTParameter 2]) ["\\a {12}  3%"] [CharToken '1' Other, CharToken '2' Other, CharToken '3' Other]
  -- \def\a#1{#1#1} \a{12} -> '1212'
  , "expands args multiple times" ~: assertExpandsTo (Def "a" [PTParameter 1] [RTParameter 1, RTParameter 1]) ["\\a{12}%"] [CharToken '1' Other, CharToken '2' Other, CharToken '1' Other, CharToken '2' Other]
  -- \def\a#1{#1} \a{{}{{}}} -> '{}{{}}'
  , "expands balanced args" ~: assertExpandsTo identDef ["\\a{{}{{}}}%"] [CharToken '{' BeginGroup, CharToken '}' EndGroup, CharToken '{' BeginGroup, CharToken '{' BeginGroup, CharToken '}' EndGroup, CharToken '}' EndGroup]
  -- \def\a#1{#1} \a{{}{{}} -> fail
  , "doesn't expand unbalanced args" ~: assertDoesntExpand identDef ["\\a{{}{{}}%"]
  -- \def\a1#1{#1} \a1a -> 'a'
  , "expands macros with leading tokens in parameters" ~: assertExpandsTo (Def "a" [PTToken (CharToken '1' Other), PTParameter 1] [RTParameter 1]) ["\\a1a%"] [CharToken 'a' Letter]
  -- \def\a1#1{#1} \a a -> fail
  , "doesn't expand macros with missing leading parameter tokens" ~: assertDoesntExpand (Def "a" [PTToken (CharToken '1' Other), PTParameter 1] [RTParameter 1]) ["\\a a%"]
  -- \def\a.#1.#2{#1#2} \a .ab -> fail
  , "doesn't expand macros with missing inner parameter tokens" ~: assertDoesntExpand dotDef ["\\a .ab%"]
  -- \def\a.#1.#2{#1#2} \a .a.b -> 'ab'
  , "expands macros with single delimited args" ~: assertExpandsTo dotDef ["\\a .a.b%"] [CharToken 'a' Letter, CharToken 'b' Letter]
  -- \def\a.#1.#2{#1#2} \a .{a}.b -> 'ab'
  , "expands macros with grouped delimited args" ~: assertExpandsTo dotDef ["\\a .{a}.b%"] [CharToken 'a' Letter, CharToken 'b' Letter]
  -- \def\a.#1.#2{#1#2} \a .{a}b.c -> '{a}bc'
  , "expands macros with multiple grouped delimited args" ~: assertExpandsTo dotDef ["\\a .{a}b.c%"] [CharToken '{' BeginGroup, CharToken 'a' Letter, CharToken '}' EndGroup, CharToken 'b' Letter, CharToken 'c' Letter]
  -- \def\a.#1.#2{#1#2} \a . a .b -> ' a b'
  , "expands macros with spaces in delimited args" ~: assertExpandsTo dotDef ["\\a . a .b%"] [CharToken ' ' Space, CharToken 'a' Letter, CharToken ' ' Space, CharToken 'b' Letter]
  -- \def\a.#1.#2{#1#2} \a .{.}a.b -> '{.}ab'
  , "expanding doesn't match parameter tokens in groups" ~: assertExpandsTo dotDef ["\\a .{.}a.b%"] [CharToken '{' BeginGroup, CharToken '.' Other, CharToken '}' EndGroup, CharToken 'a' Letter, CharToken 'b' Letter]
  -- \def\a.#1.#2{#1#2} \a ..b -> 'b'
  , "expands with empty delimited args" ~: assertExpandsTo dotDef ["\\a ..b%"] [CharToken 'b' Letter]
  ]
  where
    openBraceRT = RTToken (CharToken '{' BeginGroup)
    closeBraceRT = RTToken (CharToken '}' EndGroup)
    identDef = (Def "a" [PTParameter 1] [RTParameter 1])
    -- dotDef = \def\a.#1.#2{#1#2}
    dotDef = (Def "a" [PTToken (CharToken '.' Other), PTParameter 1, PTToken (CharToken '.' Other), PTParameter 2] [RTParameter 1, RTParameter 2])

parserTests :: Test
parserTests =
  test
  [ "basic objects" ~: assertParsesTo horizontalList ["a+%"] [HBoxChar 'a', HBoxChar '+']
  -- TODO(emily): this should actually be a glue
  , "spaces" ~: assertParsesTo horizontalList ["a %"] [HBoxChar 'a', HBoxChar ' ']
  , "stores and expands macros" ~: assertParsesTo horizontalList ["\\def\\a#1{#1#1}\\a{b}%"] [HBoxChar 'b', HBoxChar 'b']
  , "parses groups" ~: assertParsesTo horizontalList ["a{b{c}d}e%"] [HBoxChar 'a', HBoxChar 'b', HBoxChar 'c', HBoxChar 'd', HBoxChar 'e']
  , "fails on unterminated groups" ~: assertDoesntParse horizontalList ["a{b%"]
  , "fails on extra closing braces" ~: assertDoesntParse horizontalList ["a{b}}%"]
  ]

doTest :: String -> Test -> IO Bool
doTest name tests = do
  putStrLn name
  counts <- runTestTT $ name ~: tests
  return ((failures counts + errors counts) == 0)

main :: IO ()
main = do
  results <- sequence
            [ doTest "lexer" lexerTests
            , doTest "macros" macroTests
            , doTest "parser" parserTests
            ]
  if all id results
  then exitSuccess
  else exitFailure
