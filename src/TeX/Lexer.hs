module TeX.Lexer
( lex
)
where

import Prelude hiding (lex)
import qualified Data.Map as M
import qualified Data.List as L

type StateName = String
type LexInput = String
type LexOutput = String

data Matcher = EOF
             | Choice [Char]
             | Anything

data LexResultType = EOFLex | LexType String
  deriving (Show)

data TransitionType = NewState StateName Bool
                    | Fail
                    | Finish LexResultType Bool

data StateData = StateData
    { stateMatcher :: Matcher
    , stateTransition :: TransitionType
    }

data LexChar = EOFChar | NormalChar Char
  deriving (Show)

matches :: LexChar -> Matcher -> Bool
matches EOFChar EOF = True
matches _ EOF = False
matches _ Anything = True
matches (NormalChar c) (Choice choices) = elem c choices
matches _ (Choice _) = False

findMatcher :: LexChar -> [StateData] -> Maybe StateData
findMatcher c =
  L.find (\stateData -> matches c $ stateMatcher stateData)

type LexTable = M.Map StateName [StateData]

data Lexer = Lexer LexTable

mkLexer :: [(StateName, [(Matcher, TransitionType)])] -> Lexer
mkLexer = Lexer . M.fromList . getTableData
  where
    getTableData = map (\(s, d) -> (s, map (\(m, tt) -> StateData m tt) d))

data LexResult = LexResult
    { resultStr :: LexOutput
    , resultType :: LexResultType
    , resultEnd :: Int
    }
  deriving (Show)

lexRec :: Lexer -> [LexChar] -> StateName -> LexOutput -> Int -> Either String LexResult
lexRec lexer@(Lexer lexTable) (c:cs) state result pos = do
  -- Find the correct state table
  stateDatas <- case M.lookup state lexTable of
                  Just datas -> Right datas
                  Nothing -> Left $ "Couldn't find state \"" ++ state ++ "\""
  -- Find the transition corresponding to the right matcher
  transition <- case findMatcher c stateDatas of
                  Just stateData -> Right $ stateTransition stateData
                  Nothing -> Left $ "Couldn't find matcher to match '" ++ (show c) ++ "' in state \"" ++ state ++ "\""
  -- Figure out what the new resulting string is, depending on what the transition says
  newResult <- case (transition, c) of
    (Finish _ True, EOFChar) -> Left $ "Can't include EOF char in result"
    (Finish _ True, NormalChar char) -> return (char:result)
    (NewState _ True, EOFChar) -> Left $ "Can't include EOF char in result"
    (NewState _ True, NormalChar char) -> return (char:result)
    (_, _) -> return result
  -- Recurse or finish depending on transition
  case transition of
    Fail -> Left $ "Failed parsing character '" ++ (show c) ++ "' at position " ++ (show pos)
    Finish endType include -> Right $ LexResult (reverse newResult) endType $ if include then pos else pos - 1
    NewState newState _ -> lexRec lexer cs newState newResult (pos + 1)

lexAt :: Lexer -> LexInput -> Int -> Either String LexResult
lexAt lexer input pos =
  lexRec lexer lexChars "start" [] pos
  where
    lexChars = (map (\x -> NormalChar x) (L.drop pos input)) ++ [EOFChar]

lex :: Lexer -> LexInput -> Either String LexResult
lex lexer input = lexAt lexer input 0
