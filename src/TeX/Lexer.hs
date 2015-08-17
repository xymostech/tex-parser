module TeX.Lexer
( lexToken
, Lexer, mkLexer
)
where

import Prelude ( Show
               , Char, Bool, Int
               , Maybe(Just, Nothing)
               , error, otherwise, reverse, map, dropWhile
               , (==), (<=), (>=), (-), (++), (||), (&&), ($), (*), (+), (<)
               )
import qualified Data.Char as C
import Control.Lens ((^.))

import TeX.State
import TeX.Category hiding (category)
import TeX.Token

isHex :: Char -> Bool
isHex c = (c >= '0' && c <= '9') || (c >= 'a' && c <= 'f')

hexToInt :: Char -> Int
hexToInt c
  | c >= '0' && c <= '9' = (C.ord c) - (C.ord '0')
  | c >= 'a' && c <= 'f' = (C.ord c) - (C.ord 'a') + 10
  | otherwise = error ("Can't convert '" ++ [c] ++ "' from hex")

data LexerState = BeginningLine | MiddleLine | SkippingBlanks
  deriving (Show)
data Lexer = Lexer LexerState [[Char]]
  deriving (Show)

mkLexer :: [[Char]] -> Lexer
mkLexer lines =
  Lexer BeginningLine $ map mungeLines lines
  where
    mungeLines :: [Char] -> [Char]
    mungeLines line = (reverse $ dropWhile (==' ') $ reverse line) ++ ['\n']

lexNormalToken :: LexerState -> TeXState -> [Char] -> (Maybe Token, [Char], LexerState)
lexNormalToken _ _ [] = error "Encountered empty list in lexNormalToken"
lexNormalToken state texState (x:xs) =
  case (category, state) of
    (EndOfLine, BeginningLine) -> (Just $ ControlSequence "par", [], state)
    (EndOfLine, MiddleLine) -> (Just $ CharToken ' ' Space, [], state)
    (EndOfLine, SkippingBlanks) -> (Nothing, [], state)
    (Ignored, _) -> (Nothing, xs, state)
    (Space, MiddleLine) -> (Just $ CharToken ' ' Space, xs, SkippingBlanks)
    (Space, _) -> (Nothing, xs, state)
    (Comment, _) -> (Nothing, [], state)
    (Invalid, _) -> error $ "Invalid character: '" ++ [x] ++ "'"
    _ -> (Just $ CharToken x category, xs, MiddleLine)
  where
    category = texState ^. (stateCategory x)

findAllLetters :: TeXState -> [Char] -> [Char] -> ([Char], [Char])
findAllLetters _ [] result = (reverse result, [])
findAllLetters texState line@(x:xs) result
  | texState ^. (stateCategory x) == Letter = findAllLetters texState (replaceTrigraphs texState xs) (x:result)
  | otherwise = (reverse result, line)

handleEscapeSequence :: LexerState -> TeXState -> [Char] -> (Maybe Token, [Char], LexerState)
handleEscapeSequence state _ [] = (Just $ ControlSequence [], [], state)
handleEscapeSequence _ texState line@(x:xs)
  | category == Letter = (Just $ ControlSequence sequence, rest, SkippingBlanks)
  | category == Space = (Just $ ControlSequence [x], rest, SkippingBlanks)
  | otherwise = (Just $ ControlSequence [x], xs, MiddleLine)
  where
    category = texState ^. (stateCategory x)
    (sequence, rest) = findAllLetters texState (replaceTrigraphs texState line) []

lexEscapeSequence :: LexerState -> TeXState -> [Char] -> (Maybe Token, [Char], LexerState)
lexEscapeSequence _ _ [] = error "Encountered empty list in lexEscapeSequence"
lexEscapeSequence state texState line@(x:xs)
  | texState ^. (stateCategory x) == Escape = handleEscapeSequence state texState xs
  | otherwise = lexNormalToken state texState line

replaceTrigraphs :: TeXState -> [Char] -> [Char]
replaceTrigraphs texState origLine@(x:y:xs)
  | x == y &&
    texState ^. (stateCategory x) == Superscript = handleHexTrigraph xs
  | otherwise = continue
  where
    retry :: [Char] -> [Char]
    retry l = replaceTrigraphs texState l
    continue :: [Char]
    continue = origLine

    handleHexTrigraph :: [Char] -> [Char]
    handleHexTrigraph line@(z:w:zs)
      | isHex z && isHex w =
        retry $ (C.chr $ (hexToInt z * 16) + (hexToInt w)):zs
      | otherwise = handleSingleCharTrigraph line
    handleHexTrigraph line = handleSingleCharTrigraph line

    handleSingleCharTrigraph :: [Char] -> [Char]
    handleSingleCharTrigraph (z:zs)
      | c < 64 = retry $ (C.chr $ c + 64):zs
      | c < 128 = retry $ (C.chr $ c - 64):zs
      | otherwise = continue
      where
        c = C.ord z
    handleSingleCharTrigraph _ = continue
replaceTrigraphs _ line = line

startLexToken :: LexerState -> TeXState -> [Char] -> (Maybe Token, [Char], LexerState)
startLexToken state texState line =
  lexEscapeSequence state texState $ replaceTrigraphs texState line

handleLines :: Lexer -> TeXState -> Maybe (Token, Lexer)
handleLines (Lexer _ []) _ = Nothing
handleLines (Lexer _ ([]:lines)) texState =
  handleLines (Lexer BeginningLine lines) texState
handleLines (Lexer state (line:lines)) texState =
  case maybeToken of
    Just token -> Just (token, Lexer newState (newLine:lines))
    Nothing -> handleLines (Lexer newState (newLine:lines)) texState
  where
    (maybeToken, newLine, newState) = startLexToken state texState line

lexToken :: Lexer -> TeXState -> Maybe (Token, Lexer)
lexToken = handleLines
