{-# LANGUAGE Rank2Types #-}
module Main where

import Data.Either (Either(Left, Right), isLeft)
import Prelude (Char, Maybe(Just), IO, Eq, Show, String, Bool(True, False)
               , return, putStrLn, sequence, all, id, div
               , ($), (<*), (+), (==), (>>), (<), (*)
               )
import System.Exit (exitSuccess, exitFailure)
import Test.HUnit ( Assertion, Test
                  , assertEqual, assertBool, assertFailure, test, runTestTT
                  , failures, errors
                  , (~:)
                  )
import Text.Parsec (ParseError, eof, getState, anyToken)
import Control.Lens (Lens', (^.), (.~))

import TeX.Category
import TeX.Count
import TeX.Def hiding (definition)
import TeX.Lexer
import TeX.Parser.Assignment
import TeX.Parser.Conditional
import TeX.Parser.Expand
import TeX.Parser.HorizontalList
import TeX.Parser.MacroParser
import TeX.Parser.Parser
import TeX.Parser.Util
import TeX.State
import TeX.Token
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

assertDefExpandsTo :: Def -> [[Char]] -> [Token] -> Assertion
assertDefExpandsTo def lines expected =
  assertEqual "" (Right expected) (doParse (parseMacroExpansion def <* eof) lines)

assertDefDoesntExpand :: Def -> [[Char]] -> Assertion
assertDefDoesntExpand def lines =
  assertBool "parse failed" (isLeft $ doParse (parseMacroExpansion def <* eof) lines)

macroTests :: Test
macroTests =
  test
  [ "parses empty macros" ~: assertParsesTo parseDef' ["\\def\\a{}%"] (Def "a" [] [])
  , "parses basic macros" ~: assertParsesTo parseDef' ["\\def\\a{b}%"] (Def "a" [] [RTToken (CharToken 'b' Letter)])
  , "parses macros with arguments" ~: assertParsesTo parseDef' ["\\def\\a#1{}%"] (Def "a" [PTParameter 1] [])
  , "parses macros with multiple arguments" ~: assertParsesTo parseDef' ["\\def\\a#1#2#3{}%"] (Def "a" [PTParameter 1, PTParameter 2, PTParameter 3] [])
  , "doesn't parse macros with backwards args" ~: assertDoesntParse parseDef' ["\\def\\a#2#1{}%"]
  , "doesn't parse macros with out-of-order args" ~: assertDoesntParse parseDef' ["\\def\\a#1#3{}%"]
  , "doesn't parse macros with duplicate args" ~: assertDoesntParse parseDef' ["\\def\\a#1#1{}%"]
  , "parses macros with args in their bodies" ~: assertParsesTo parseDef' ["\\def\\a#1{#1}%"] (Def "a" [PTParameter 1] [RTParameter 1])
  , "parses macros with multiple args in their bodies" ~: assertParsesTo parseDef' ["\\def\\a#1#2{#2#1#1#2}%"] (Def "a" [PTParameter 1, PTParameter 2] [RTParameter 2, RTParameter 1, RTParameter 1, RTParameter 2])
  , "parses macros with balanced bodies" ~: assertParsesTo parseDef' ["\\def\\a{{}{{}}}%"] (Def "a" [] [openBraceRT, closeBraceRT, openBraceRT, openBraceRT, closeBraceRT, closeBraceRT])
  , "doesn't parse macros with imbalanced bodies" ~: assertDoesntParse parseDef' ["\\def\\a{{}{{}}%"]
  , "parses double pound signs" ~: assertParsesTo parseDef' ["\\def\\a{##}%"] (Def "a" [] [RTToken (CharToken '#' Parameter)])
  , "parses other tokens in the replacement text" ~: assertParsesTo parseDef' ["\\def\\a{a#1 \\x##}%"] (Def "a" [] [RTToken (CharToken 'a' Letter), RTParameter 1, RTToken (CharToken ' ' Space), RTToken (ControlSequence "x"), RTToken (CharToken '#' Parameter)])
  , "parses macros with newlines" ~: assertParsesTo parseDef' ["\\def\\a{", "}%"] (Def "a" [] [RTToken (CharToken ' ' Space)])
  , "parses macros with tokens in the parameters" ~: assertParsesTo parseDef' ["\\def\\a 1#12{}%"] (Def "a" [PTToken (CharToken '1' Other), PTParameter 1, PTToken (CharToken '2' Other)] [])
  , "parses trailing parameter braces" ~: assertParsesTo parseDef' ["\\def\\a #1#{}%"] (Def "a" [PTParameter 1, PTTrailingBrace] [])
  , "doesn't parse parameters in the replacement text" ~: assertDoesntParse parseDef' ["\\def\\a{#}%"]
  -- \def\a{} \a -> ''
  , "expands empty macros" ~: assertDefExpandsTo (Def "a" [] []) ["\\a%"] []
  -- \def\a{ab} \a -> 'ab'
  , "expands no-arg macros" ~: assertDefExpandsTo (Def "a" [] [RTToken (CharToken 'a' Letter), RTToken (CharToken 'b' Letter)]) ["\\a%"] [CharToken 'a' Letter, CharToken 'b' Letter]
  -- \def\a#1{a#1b} \a{} -> 'ab'
  , "expands empty groups" ~: assertDefExpandsTo (Def "a" [PTParameter 1] [RTToken (CharToken 'a' Letter), RTParameter 1, RTToken (CharToken 'b' Letter)]) ["\\a{}%"] [CharToken 'a' Letter, CharToken 'b' Letter]
  -- \def\a#1{#1} \a1 -> '1'
  , "expands single argument macros with single token arg" ~: assertDefExpandsTo (Def "a" [PTParameter 1] [RTParameter 1]) ["\\a1%"] [CharToken '1' Other]
  -- \def\a#1{#1} \a{ab} -> 'ab'
  , "expands single argument macros with a group arg" ~: assertDefExpandsTo (Def "a" [PTParameter 1] [RTParameter 1]) ["\\a{ab}%"] [CharToken 'a' Letter, CharToken 'b' Letter]
  -- \def\a#1#2{#1#2} \a {12}3 -> '123'
  , "expands multiple argument macros" ~: assertDefExpandsTo (Def "a" [PTParameter 1, PTParameter 2] [RTParameter 1, RTParameter 2]) ["\\a {12}3%"] [CharToken '1' Other, CharToken '2' Other, CharToken '3' Other]
  -- \def\a#1#2{#1#2} \a {12}  3 -> '123'
  , "expands multiple arg macros ignoring spaces" ~: assertDefExpandsTo (Def "a" [PTParameter 1, PTParameter 2] [RTParameter 1, RTParameter 2]) ["\\a {12}  3%"] [CharToken '1' Other, CharToken '2' Other, CharToken '3' Other]
  -- \def\a#1{#1#1} \a{12} -> '1212'
  , "expands args multiple times" ~: assertDefExpandsTo (Def "a" [PTParameter 1] [RTParameter 1, RTParameter 1]) ["\\a{12}%"] [CharToken '1' Other, CharToken '2' Other, CharToken '1' Other, CharToken '2' Other]
  -- \def\a#1{#1} \a{{}{{}}} -> '{}{{}}'
  , "expands balanced args" ~: assertDefExpandsTo identDef ["\\a{{}{{}}}%"] [CharToken '{' BeginGroup, CharToken '}' EndGroup, CharToken '{' BeginGroup, CharToken '{' BeginGroup, CharToken '}' EndGroup, CharToken '}' EndGroup]
  -- \def\a#1{#1} \a{{}{{}} -> fail
  , "doesn't expand unbalanced args" ~: assertDefDoesntExpand identDef ["\\a{{}{{}}%"]
  -- \def\a1#1{#1} \a1a -> 'a'
  , "expands macros with leading tokens in parameters" ~: assertDefExpandsTo (Def "a" [PTToken (CharToken '1' Other), PTParameter 1] [RTParameter 1]) ["\\a1a%"] [CharToken 'a' Letter]
  -- \def\a1#1{#1} \a a -> fail
  , "doesn't expand macros with missing leading parameter tokens" ~: assertDefDoesntExpand (Def "a" [PTToken (CharToken '1' Other), PTParameter 1] [RTParameter 1]) ["\\a a%"]
  -- \def\a.#1.#2{#1#2} \a .ab -> fail
  , "doesn't expand macros with missing inner parameter tokens" ~: assertDefDoesntExpand dotDef ["\\a .ab%"]
  -- \def\a.#1.#2{#1#2} \a .a.b -> 'ab'
  , "expands macros with single delimited args" ~: assertDefExpandsTo dotDef ["\\a .a.b%"] [CharToken 'a' Letter, CharToken 'b' Letter]
  -- \def\a.#1.#2{#1#2} \a .{a}.b -> 'ab'
  , "expands macros with grouped delimited args" ~: assertDefExpandsTo dotDef ["\\a .{a}.b%"] [CharToken 'a' Letter, CharToken 'b' Letter]
  -- \def\a.#1.#2{#1#2} \a .{a}b.c -> '{a}bc'
  , "expands macros with multiple grouped delimited args" ~: assertDefExpandsTo dotDef ["\\a .{a}b.c%"] [CharToken '{' BeginGroup, CharToken 'a' Letter, CharToken '}' EndGroup, CharToken 'b' Letter, CharToken 'c' Letter]
  -- \def\a.#1.#2{#1#2} \a . a .b -> ' a b'
  , "expands macros with spaces in delimited args" ~: assertDefExpandsTo dotDef ["\\a . a .b%"] [CharToken ' ' Space, CharToken 'a' Letter, CharToken ' ' Space, CharToken 'b' Letter]
  -- \def\a.#1.#2{#1#2} \a .{.}a.b -> '{.}ab'
  , "expanding doesn't match parameter tokens in groups" ~: assertDefExpandsTo dotDef ["\\a .{.}a.b%"] [CharToken '{' BeginGroup, CharToken '.' Other, CharToken '}' EndGroup, CharToken 'a' Letter, CharToken 'b' Letter]
  -- \def\a.#1.#2{#1#2} \a ..b -> 'b'
  , "expands with empty delimited args" ~: assertDefExpandsTo dotDef ["\\a ..b%"] [CharToken 'b' Letter]
  ]
  where
    parseDef' = parseDef noExpand

    openBraceRT = RTToken (CharToken '{' BeginGroup)
    closeBraceRT = RTToken (CharToken '}' EndGroup)
    identDef = (Def "a" [PTParameter 1] [RTParameter 1])
    -- dotDef = \def\a.#1.#2{#1#2}
    dotDef = (Def "a" [PTToken (CharToken '.' Other), PTParameter 1, PTToken (CharToken '.' Other), PTParameter 2] [RTParameter 1, RTParameter 2])

assertStateReturns :: (Eq b, Show b) => TeXParser a -> [[Char]] -> (Lens' TeXState b) -> b -> Assertion
assertStateReturns parser lines accessor expected =
  case eitherFinalState of
    Left _ -> assertFailure "parsing failed"
    Right finalState -> assertEqual "" expected (finalState ^. accessor)
  where
    eitherFinalState = runParser (parser >> eof >> getState) (Just $ mkState myParserMap) lines

stateTests :: Test
stateTests =
  test
  [ "defs are assigned" ~: assertStateReturns assignment' ["\\def\\a {1}%"] (stateDefinition "a") (Just $ Def "a" [] [RTToken (CharToken '1' Other)])
  , "defs allow expansion" ~: assertStateReturns (assignment' >> assignment') ["\\def\\a{\\def\\b{c}}\\a%"] (stateDefinition "b") (Just $ Def "b" [] [RTToken (CharToken 'c' Letter)])
  , "counts are assigned" ~: assertStateReturns assignment' ["\\count0=1%"] (stateCount 0) (Just 1)
  , "counts allow extra whitespace" ~: assertStateReturns assignment' ["\\count0   =   1 %"] (stateCount 0) (Just 1)
  , "counts don't need = signs" ~: assertStateReturns assignment' ["\\count0 1%"] (stateCount 0) (Just 1)
  , "counts allow expansion" ~: assertStateReturns (assignment' >> assignment') ["\\def\\a{\\count0=1}\\a%"] (stateCount 0) (Just 1)
  , "count counters allow expansion" ~: assertStateReturns (assignment' >> assignment') ["\\def\\a{0}\\count\\a\\a\\a=1%"] (stateCount 0) (Just 1)
  , "count equals allows expansion" ~: assertStateReturns (assignment' >> assignment') ["\\def\\a{=}\\count0\\a1%"] (stateCount 0) (Just 1)
  , "count value allows expansion" ~: assertStateReturns (assignment' >> assignment') ["\\def\\a{10}\\count0=\\a\\a%"] (stateCount 0) (Just 1010)
  ]
  where
    assignment' = assignment expand

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

  , "fails to expand if expanders fail" ~: assertDoesntParse (expand anyToken) ["\\ifnum%"]
  , "succeeds if expanders don't fail" ~: assertParsesTo (expand anyToken) ["\\a"] (ControlSequence "a")
  ]

conditionalTests :: Test
conditionalTests =
  test
  [ "iftrue heads parse" ~: assertParsesTo (conditionalHead noExpand) ["\\iftrue%"] IfTrue
  , "ifnum heads parse" ~: assertParsesTo (conditionalHead noExpand) ["\\ifnum 2<3%"] (IfNum (LitCount 2) LessThan (LitCount 3))

  , "number comparisons are parsed correctly" ~: assertParsesTo (conditionalHead noExpand) ["\\ifnum 2<3%"] (IfNum (LitCount 2) LessThan (LitCount 3))
  , "number comparisons parse counts correctly" ~: assertParsesTo (conditionalHead noExpand) ["\\ifnum \\count0<3%"] (IfNum (IntVar $ LiteralCount 0) LessThan (LitCount 3))
  , "number comparisons expand" ~: assertParsesTo (assignment expand >> conditionalHead expand) ["\\def\\a{1}\\ifnum\\a=\\a%"] (IfNum (LitCount 1) EqualTo (LitCount 1))

  , "iftrue heads evaluate true" ~: assertParsesTo (evaluateHead IfTrue) [] True
  , "iffalse heads evaluate true" ~: assertParsesTo (evaluateHead IfFalse) [] False
  , "< heads evaluate correctly" ~: assertParsesTo (evaluateHead (IfNum (LitCount 3) LessThan (LitCount 2))) [] False
  , "> heads evaluate correctly" ~: assertParsesTo (evaluateHead (IfNum (LitCount 3) GreaterThan (LitCount 2))) [] True
  , "= heads evaluate correctly" ~: assertParsesTo (evaluateHead (IfNum (LitCount 5) EqualTo (LitCount 5))) [] True

  , "comparison heads expand \\counts" ~: assertParsesTo (assignment expand >> evaluateHead (IfNum (IntVar (LiteralCount 0)) EqualTo (LitCount 4))) ["\\count0=4%"] True

  , "finds true body" ~: assertParsesTo (runConditionalBody noExpand True) ["a\\fi%"] [CharToken 'a' Letter]
  , "finds true body with else body" ~: assertParsesTo (runConditionalBody noExpand True) ["a\\else b\\fi%"] [CharToken 'a' Letter]
  , "finds false body" ~: assertParsesTo (runConditionalBody noExpand False) ["a\\else b\\fi%"] [CharToken 'b' Letter]
  , "finds false body with no else" ~: assertParsesTo (runConditionalBody noExpand False) ["a\\fi%"] []

  , "expands nested ifs in true" ~: assertParsesTo (runConditionalBody expand True) ["a\\iftrue b\\fi\\fi%"] [CharToken 'a' Letter, CharToken 'b' Letter]
  , "ignores nested ifs in false" ~: assertParsesTo (runConditionalBody expand True) ["a\\else\\iftrue b\\fi\\fi%"] [CharToken 'a' Letter]
  , "ignores nested ifs in true" ~: assertParsesTo (runConditionalBody expand False) ["\\iftrue a\\fi\\else b\\fi%"] [CharToken 'b' Letter]
  , "expands nested ifs in false" ~: assertParsesTo (runConditionalBody expand False) ["a\\else\\iftrue b\\fi\\fi%"] [CharToken 'b' Letter]

  , "bad defs fail inside ifs" ~: assertDoesntParse (runConditionalBody expand True) ["\\def\\a\\fi%"]
  , "defs expand in ifs" ~: assertStateReturns (runConditionalBody expand True) ["\\def\\a{b}\\fi%"] (stateDefinition "a") (Just $ Def "a" [] [RTToken (CharToken 'b' Letter)])
  , "setters expand in ifs" ~: assertStateReturns (runConditionalBody expand True) ["\\count0=1\\fi%"] (stateCount 0) (Just 1)

  , "conditionals expand" ~: assertParsesTo (expandConditional expand) ["\\iftrue a\\fi%"] [CharToken 'a' Letter]
  , "conditional else expands" ~: assertParsesTo (expandConditional expand) ["\\iffalse a\\else b\\fi%"] [CharToken 'b' Letter]
  , "conditionals evaluate and expand" ~: assertParsesTo (expandConditional expand) ["\\ifnum 2>3 a\\else b\\fi%"] [CharToken 'b' Letter]
  ]

utilTests :: Test
utilTests =
  test
  [ "empty strings don't parse to numbers" ~: assertDoesntParse (number noExpand) []
  , "numbers parse to numbers" ~: assertParsesTo (number noExpand) ["213%"] 213
  , "counts are comparable" ~: assertBool "" ((3 :: Count) < (4 :: Count))
  , "counts do math correctly" ~: assertBool "" (((20 :: Count) `div` (3 :: Count)) + ((2 :: Count) * (3 :: Count)) == (12 :: Count))
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
            , doTest "state" stateTests
            , doTest "parser" parserTests
            , doTest "conditional" conditionalTests
            , doTest "util" utilTests
            ]
  if all id results
  then exitSuccess
  else exitFailure
