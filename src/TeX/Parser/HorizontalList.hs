module TeX.Parser.HorizontalList
( HorizontalListElem(HBoxChar, HPenalty)
, horizontalList, horizontalListChars
)
where

import Prelude ( Char, Int, Show, Eq
               , map, undefined, return
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
  (expand $ categoryToken BeginGroup >> beginGroup) *>
   horizontalList <*
   (expand $ categoryToken EndGroup >> endGroup)

horizontalList :: TeXParser [HorizontalListElem]
horizontalList = do
  option [] $ (assignment expand >> horizontalList) <|>
              ((:) <$> horizontalListElem <*> horizontalList) <|>
              ((++) <$> groupedHorizontalList <*> horizontalList)

-- A testing parser that spits out the characters we get as output.
horizontalListChars :: TeXParser [Char]
horizontalListChars = do
  list <- horizontalList
  return $ map toChar list
  where
    toChar (HBoxChar c) = c
    toChar _ = undefined
