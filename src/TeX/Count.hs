module TeX.Count
( Count(CountOverflow)
)
where

data Count = Count Integer | CountOverflow
  deriving (Eq, Show)

checkOverflowed :: Count -> Count
checkOverflowed (Count num)
  | num < -2147483647 = CountOverflow
  | num > 2147483647 = CountOverflow
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
