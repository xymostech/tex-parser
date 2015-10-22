module TeX.Count
( Count(CountOverflow)
)
where

data Count = Count Integer | CountOverflow
  deriving (Eq, Show)

maxValue :: Integer
maxValue = 2147483647
minValue :: Integer
minValue = -2147483647

checkOverflowed :: Count -> Count
checkOverflowed (Count num)
  | num < minValue = CountOverflow
  | num > maxValue = CountOverflow
  | otherwise = (Count num)
checkOverflowed CountOverflow = CountOverflow

instance Num Count where
  (+) (Count a) (Count b) = checkOverflowed $ Count (a + b)
  (+) _ _ = CountOverflow

  (-) (Count a) (Count b) = checkOverflowed $ Count (a - b)
  (-) _ _ = CountOverflow

  (*) (Count a) (Count b) = checkOverflowed $ Count (a * b)
  (*) _ _ = CountOverflow

  negate (Count a) = Count $ negate a
  negate _ = CountOverflow

  abs (Count a) = Count $ abs a
  abs _ = CountOverflow

  signum (Count a) = Count $ signum a
  signum _ = CountOverflow

  fromInteger a = checkOverflowed $ Count a

instance Enum Count where
  succ (Count a)
    | a == maxValue = Count minValue
    | otherwise = Count (a + 1)
  succ CountOverflow = CountOverflow

  pred (Count a)
    | a == minValue = Count maxValue
    | otherwise = Count (a - 1)
  pred CountOverflow = CountOverflow

  fromEnum (Count a) = fromInteger a
  fromEnum CountOverflow = fromInteger $ maxValue + 1

  toEnum num = checkOverflowed (Count $ fromIntegral num)

instance Real Count where
  toRational (Count a) = toRational a
  toRational CountOverflow = (1 / 0)

instance Integral Count where
  quotRem (Count a) (Count b) =
    (checkOverflowed $ Count (quot a b),
     checkOverflowed $ Count (rem a b))
  quotRem _ _ = (CountOverflow, CountOverflow)

  toInteger (Count a) = toInteger a
  toInteger CountOverflow = undefined

instance Ord Count where
  (<=) (Count a) (Count b) = a < b
  (<=) (Count _) CountOverflow = True
  (<=) CountOverflow (Count _) = False
  (<=) CountOverflow CountOverflow = False
