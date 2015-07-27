import Prelude hiding (lex)
import qualified Data.List as L

import TeX.Lexer

letters = ['a'..'z'] ++ ['A'..'Z']

mainTable =
  [ ("start",
     [ (Choice [' ', '\t', '\n'], NewState "start" False)
     , (Choice letters, NewState "word" True)
     , (Choice ['0'..'9'], NewState "number-head" True)
     , (EOF, Finish EOFLex False)
     , (Anything, Fail)
     ]
    )
  , ("word",
     [ (Choice letters, NewState "word" True)
     , (Anything, Finish (LexType "word") False)
     ]
    )
  , ("number-head",
     [ (Choice ['0'..'9'], NewState "number-head" True)
     , (Choice ['.'], NewState "number-tail" True)
     , (Anything, Finish (LexType "number") False)
     ]
    )
  , ("number-tail",
     [ (Choice ['0'..'9'], NewState "number-tail" True)
     , (Anything, Finish (LexType "number") False)
     ]
    )
  ]

lexAll :: Lexer -> String -> Either String [LexResult]
lexAll lexer str = do
  result <- lex lexer str
  case resultType result of
    EOFLex -> return [result]
    _ -> do
      rest <- lexAll lexer (drop (resultEnd result + 1) str)
      return (result:rest)

main :: IO ()
main = do
  let lexer = mkLexer mainTable
  print $ lexAll lexer "   123abc"
