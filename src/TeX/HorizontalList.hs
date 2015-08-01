module TeX.HorizontalList where

import Prelude ( Char, Int, Show
               , (<$>), (>>), (<*)
               )
import Text.Parsec

import TeX.Parser
import TeX.Category
import TeX.Token
import TeX.MacroParser

data HorizontalListElem
  = HBoxChar Char
  | HPenalty Int
  deriving (Show)

type HorizontalList = [HorizontalListElem]

parseHorizontalListElem :: TeXParser HorizontalListElem
parseHorizontalListElem =
  HBoxChar <$> (extractChar <$> (categoryToken Letter)) <|>
  HBoxChar <$> (extractChar <$> (categoryToken Other)) <|>
  HBoxChar <$> (extractChar <$> (categoryToken Space)) <|>
  (parseMacros >> parseHorizontalListElem)
  <?> "horizontal list elem"

parseHorizontalList :: TeXParser HorizontalList
parseHorizontalList =
  (many parseHorizontalListElem) <* eof
