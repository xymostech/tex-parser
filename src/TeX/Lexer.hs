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

import TeX.Category as Cat
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

lexNormalToken :: LexerState -> CategoryMap -> [Char] -> (Maybe Token, [Char], LexerState)
lexNormalToken _ _ [] = error "Encountered empty list in lexNormalToken"
lexNormalToken state catMap (x:xs) =
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
    category = Cat.lookup x catMap

findAllLetters :: CategoryMap -> [Char] -> [Char] -> ([Char], [Char])
findAllLetters _ [] result = (reverse result, [])
findAllLetters catMap line@(x:xs) result
  | Cat.lookup x catMap == Letter = findAllLetters catMap (replaceTrigraphs catMap xs) (x:result)
  | otherwise = (reverse result, line)

handleEscapeSequence :: LexerState -> CategoryMap -> [Char] -> (Maybe Token, [Char], LexerState)
handleEscapeSequence state _ [] = (Just $ ControlSequence [], [], state)
handleEscapeSequence _ catMap line@(x:xs)
  | category == Letter = (Just $ ControlSequence sequence, rest, SkippingBlanks)
  | category == Space = (Just $ ControlSequence [x], rest, SkippingBlanks)
  | otherwise = (Just $ ControlSequence [x], xs, MiddleLine)
  where
    category = Cat.lookup x catMap
    (sequence, rest) = findAllLetters catMap (replaceTrigraphs catMap line) []

lexEscapeSequence :: LexerState -> CategoryMap -> [Char] -> (Maybe Token, [Char], LexerState)
lexEscapeSequence _ _ [] = error "Encountered empty list in lexEscapeSequence"
lexEscapeSequence state catMap line@(x:xs)
  | Cat.lookup x catMap == Escape = handleEscapeSequence state catMap xs
  | otherwise = lexNormalToken state catMap line

replaceTrigraphs :: CategoryMap -> [Char] -> [Char]
replaceTrigraphs catMap origLine@(x:y:xs)
  | x == y &&
    Cat.lookup x catMap == Superscript = handleHexTrigraph xs
  | otherwise = continue
  where
    retry :: [Char] -> [Char]
    retry l = replaceTrigraphs catMap l
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

startLexToken :: LexerState -> CategoryMap -> [Char] -> (Maybe Token, [Char], LexerState)
startLexToken state catMap line =
  lexEscapeSequence state catMap $ replaceTrigraphs catMap line

handleLines :: Lexer -> CategoryMap -> Maybe (Token, Lexer)
handleLines (Lexer _ []) _ = Nothing
handleLines (Lexer _ ([]:lines)) catMap =
  handleLines (Lexer BeginningLine lines) catMap
handleLines (Lexer state (line:lines)) catMap =
  case maybeToken of
    Just token -> Just (token, Lexer newState (newLine:lines))
    Nothing -> handleLines (Lexer newState (newLine:lines)) catMap
  where
    (maybeToken, newLine, newState) = startLexToken state catMap line

lexToken :: Lexer -> CategoryMap -> Maybe (Token, Lexer)
lexToken = handleLines
