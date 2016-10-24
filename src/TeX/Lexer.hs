{-# LANGUAGE MultiParamTypeClasses #-}
module TeX.Lexer
( Lexer, mkLexer
)
where

import Prelude ( Show, Eq
               , Char, Bool, Int
               , Maybe(Just, Nothing)
               , error, otherwise, reverse, map, dropWhile
               , (==), (<=), (>=), (-), (++), (||), (&&), ($), (*), (+), (<), (/=), (<$>)
               )
import qualified Data.Char as C
import Control.Arrow (first)
import Control.Lens ((^.))

import TeX.State
import TeX.Category hiding (category)
import TeX.Token
import TeX.Iterator

-- LINES

data LinesValue = LineEndOfLine | LineChar Char
  deriving (Show, Eq)
newtype Lines = Lines [[Char]]
  deriving (Show)

instance Iterator Lines LinesValue where
    next (Lines []) = (Nothing, Lines [])
    next (Lines ([]:ls)) = (Just LineEndOfLine, Lines ls)
    next (Lines ((x:xs):ls)) = (Just $ LineChar x, Lines (xs:ls))

-- TRIGRAPH EXPANDER

newtype TrigraphExpander = TrigraphExpander Lines
  deriving (Show)

getChar :: TrigraphExpander -> Maybe (Char, TrigraphExpander)
getChar (TrigraphExpander lines) =
    case l of
      Nothing -> Nothing
      Just LineEndOfLine -> Nothing
      Just (LineChar c) -> Just (c, TrigraphExpander lines')
    where
      (l, lines') = next lines

expand :: Char -> TrigraphExpander -> TeXState -> (Char, TrigraphExpander)
expand c te texState =
    case getCategory c of
      -- First ^
      Superscript ->
          case (first getCategory) <$> (getChar te) of
            -- Second ^
            Just (Superscript, te') ->
                case getChar te' of
                  -- A character
                  Just (c, te'') ->
                      -- hex character
                      if isHex c
                      then
                          case getChar te'' of
                            Just (d, te''') ->
                                -- second hex character
                                if isHex d
                                then expand (expandHex c d) (te''') texState
                                else expand (expandChar c) (te'') texState
                            _ -> expand (expandChar c) (te'') texState
                      else expand (expandChar c) (te'') texState
                  _ -> (c, te)
            _ -> (c, te)
      _ -> (c, te)
    where
      getCategory c = texState ^. (stateCategory c)

      isHex :: Char -> Bool
      isHex c = (c >= '0' && c <= '9') || (c >= 'a' && c <= 'f')

      hexToInt :: Char -> Int
      hexToInt c
        | c >= '0' && c <= '9' = (C.ord c) - (C.ord '0')
        | c >= 'a' && c <= 'f' = (C.ord c) - (C.ord 'a') + 10
        | otherwise = error ("Can't convert '" ++ [c] ++ "' from hex")

      expandHex :: Char -> Char -> Char
      expandHex x y = C.chr $ (hexToInt x) * 16 + (hexToInt y)

      expandChar :: Char -> Char
      expandChar c
          | code < 64  = C.chr $ code + 64
          | code < 128 = C.chr $ code - 64
          | otherwise  = error "Trigraphs don't support >127 charcters"
          where
            code = C.ord c

instance Iterator TrigraphExpander LinesValue where
    next (TrigraphExpander lines) =
        case l of
          Nothing -> (Nothing, TrigraphExpander lines')
          Just LineEndOfLine -> (Just LineEndOfLine, TrigraphExpander lines')
          Just (LineChar c) ->
              let (c', te'') = expand c (TrigraphExpander lines') texState
              in (Just $ LineChar c', te'')
        where
          (l, lines') = next lines
          texState = mkState initialMap

-- LEXER

data LexerState = BeginningLine | MiddleLine | SkippingBlanks
  deriving (Show)

data Lexer = Lexer LexerState TrigraphExpander
  deriving (Show)

mkLexer :: [[Char]] -> Lexer
mkLexer lines =
  Lexer BeginningLine (TrigraphExpander $ Lines $ map mungeLines lines)
  where
    mungeLines :: [Char] -> [Char]
    mungeLines line = (reverse $ dropWhile (==' ') $ reverse line) ++ ['\n']

findAllLetters :: Lexer -> TeXState -> [Char] -> (Maybe Token, Lexer)
findAllLetters lexer@(Lexer state lines) texState result =
    case c of
      Nothing -> (Just $ ControlSequence $ reverse result, lexer)
      Just LineEndOfLine -> (Just $ ControlSequence $ reverse result, lexer)
      Just (LineChar c')
           | texState ^. (stateCategory c') == Letter -> findAllLetters (Lexer state lines') texState (c':result)
           | otherwise -> (Just $ ControlSequence $ reverse result, lexer)
    where
      (c, lines') = next lines

lexEscapeSequence' :: Lexer -> TeXState -> (Maybe Token, Lexer)
lexEscapeSequence' (Lexer state lines) texState =
    case c of
      Nothing -> (Just $ ControlSequence [], Lexer state lines')
      Just LineEndOfLine -> (Just $ ControlSequence [], Lexer state lines')
      Just (LineChar c')
          | getCategory c' == Space -> (Just $ ControlSequence [c'], Lexer SkippingBlanks lines')
          | getCategory c' == Letter -> findAllLetters (Lexer SkippingBlanks lines') texState [c']
          | otherwise -> (Just $ ControlSequence [c'], Lexer MiddleLine lines')
    where
      getCategory c = texState ^. (stateCategory c)
      (c, lines') = next lines

lexEscapeSequence :: Char -> Lexer -> TeXState -> (Maybe Token, Lexer)
lexEscapeSequence c lexer texState
    | texState ^. (stateCategory c) == Escape = lexEscapeSequence' lexer texState
    | otherwise = (Nothing, lexer)

lexNormalToken :: Char -> Lexer -> TeXState -> (Maybe Token, Lexer)
lexNormalToken c (Lexer state lines) texState =
    case (texState ^. (stateCategory c), state) of
      (EndOfLine, BeginningLine) -> (Just $ ControlSequence "par", Lexer state lines)
      (EndOfLine, MiddleLine) -> (Just $ CharToken ' ' Space, Lexer state clearedLine)
      (EndOfLine, SkippingBlanks) -> (Nothing, Lexer state clearedLine)
      (Ignored, _) -> (Nothing, Lexer state lines)
      (Space, MiddleLine) -> (Just $ CharToken ' ' Space, Lexer SkippingBlanks lines)
      (Space, _) -> (Nothing, Lexer state lines)
      (Comment, _) -> (Nothing, Lexer state clearedLine)
      (Invalid, _) -> error $ "Invalid character: '" ++ [c] ++ "'"
      (category, _) -> (Just $ CharToken c category, Lexer MiddleLine lines)
    where
      clearedLine = dropWhileIt lines (/=LineEndOfLine)

instance Iterator Lexer Token where
  next lexer@(Lexer state lines) =
      case l of
        Nothing -> (Nothing, Lexer state lines')
        Just LineEndOfLine -> next (Lexer BeginningLine lines')
        Just (LineChar c) ->
            case lexEscapeSequence c (Lexer state lines') texState of
              (Just tok, lexer') -> (Just tok, lexer')
              (Nothing, lexer') ->
                  case lexNormalToken c lexer' texState of
                    (Nothing, lexer'') -> next lexer''
                    (Just tok, lexer'') -> (Just tok, lexer'')
      where
        (l, lines') = next lines
        texState = mkState initialMap
