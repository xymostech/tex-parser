module TeX.HorizontalList
( HorizontalListElem(HBoxChar, HPenalty)
, horizontalList
)
where

import Prelude ( Char, Int, Show, Eq
               , (<$>), (>>), (<*>), ($)
               )
import Text.Parsec

import TeX.Parser
import TeX.Category
import TeX.Token
import TeX.MacroParser

data HorizontalListElem
  = HBoxChar Char
  | HPenalty Int
  deriving (Eq, Show)

horizontalListElem :: TeXParser HorizontalListElem
horizontalListElem =
  HBoxChar <$> (extractChar <$> (categoryToken Letter)) <|>
  HBoxChar <$> (extractChar <$> (categoryToken Other)) <|>
  HBoxChar <$> (extractChar <$> (categoryToken Space))
  <?> "horizontal list elem"

horizontalList :: TeXParser [HorizontalListElem]
horizontalList = do
  option [] $ ((:) <$> horizontalListElem <*> horizontalList) <|>
              (parseMacros >> horizontalList)
