module TeX.Category
( Category( Escape, BeginGroup
          , EndGroup, MathShift
          , AlignmentTab, EndOfLine
          , Parameter, Superscript
          , Subscript, Ignored
          , Space, Letter
          , Other, Active
          , Comment, Invalid
          )
, fromCategory, toCategory
, CategoryMap()
, empty, lookup, set
, initialMap
)
where

import Prelude ( Show, Enum, Eq
               , Int, Char
               , toEnum, fromEnum
               , Maybe(Just, Nothing)
               , ($), (==), (++))
import qualified Data.Map as M

data Category = Escape
              | BeginGroup
              | EndGroup
              | MathShift
              | AlignmentTab
              | EndOfLine
              | Parameter
              | Superscript
              | Subscript
              | Ignored
              | Space
              | Letter
              | Other
              | Active
              | Comment
              | Invalid
  deriving (Show, Enum, Eq)

fromCategory :: Category -> Int
fromCategory = fromEnum

toCategory :: Int -> Category
toCategory = toEnum

data CategoryMap = CategoryMap (M.Map Char Category)
  deriving (Show)

lookup :: Char -> CategoryMap -> Category
lookup c (CategoryMap map) =
  case M.lookup c map of
    Just cat -> cat
    Nothing -> Other

empty :: CategoryMap
empty = CategoryMap M.empty

set :: Char -> Category -> CategoryMap -> CategoryMap
set c cat (CategoryMap map) =
  CategoryMap $ if cat == Other
                then M.delete c map
                else M.insert c cat map

initialMap :: CategoryMap
initialMap =
  CategoryMap $ M.fromList $
                [ ('\\', Escape)
                , ('\0', Ignored)
                , ('%', Comment)
                , ('\n', EndOfLine)
                , (' ', Space)
                ] ++
                [ (c, Letter) | c <- ['a'..'z'] ] ++
                [ (c, Letter) | c <- ['A'..'Z'] ]
