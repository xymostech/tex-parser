module TeX.Parser.HorizontalList
( HorizontalListElem(HBoxChar, HPenalty)
, horizontalList
)
where

import Prelude ( Char, Int, Show, Eq
               , (<$>), (>>), (<*>), ($), (<*), (*>), (++)
               )
import Text.Parsec

import TeX.Category
import TeX.Parser.Expand
import TeX.Parser.Parser
import TeX.Parser.Prim
import TeX.Token
import TeX.Parser.Assignment

data HorizontalListElem
  = HBoxChar Char
  | HPenalty Int
  deriving (Eq, Show)

horizontalListElem :: TeXParser HorizontalListElem
horizontalListElem =
  expand
  (HBoxChar <$> (extractChar <$> (categoryToken Letter)) <|>
   HBoxChar <$> (extractChar <$> (categoryToken Other)) <|>
   HBoxChar <$> (extractChar <$> (categoryToken Space))
   <?> "horizontal list elem")

groupedHorizontalList :: TeXParser [HorizontalListElem]
groupedHorizontalList =
  (expand $ categoryToken BeginGroup) *>
   horizontalList <*
   (expand $ categoryToken EndGroup)

horizontalList :: TeXParser [HorizontalListElem]
horizontalList = do
  option [] $ (assignment >> horizontalList) <|>
              ((:) <$> horizontalListElem <*> horizontalList) <|>
              ((++) <$> groupedHorizontalList <*> horizontalList)
