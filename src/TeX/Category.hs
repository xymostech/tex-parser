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
, empty, category
, initialMap
)
where

import Prelude ( Show, Enum, Eq, Functor
               , Int, Char
               , toEnum, fromEnum
               , Maybe()
               , ($), (++))
import qualified Data.Map as M

import TeX.StateUtils

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

newtype CategoryMap = CategoryMap (M.Map Char Category)
  deriving (Show)

--category :: Char -> Lens' CategoryMap (Maybe Category)
category :: Functor f => Char -> (Maybe Category -> f (Maybe Category)) ->
            CategoryMap -> f CategoryMap
category = makeLens (\(CategoryMap x) -> x) CategoryMap

empty :: CategoryMap
empty = CategoryMap M.empty

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
