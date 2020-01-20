module RadixArithmetic where

import Data.Char
import NumberRepresentation


-- Function: radixAddition (FirstValue, SecondValue,Base)
-- Pre: all values must be positive, must be in the integer set
-- This function is quite complicated because it converts between ints and lists. Firstly the initial function makes
-- sure that the digit length of the two inputs are the same, if they're not then it adds leading zeros to the smaller
-- function. then the main function applies a helper method to complete the radix addition rule to the values.
-- the main function then takes the value from the helper method and applies the concat function to it.
radixAddition :: (Int, Int, Int) -> Int
radixAddition (fstValue, sndValue, base)
  | length1 > length2     = radixAddition (intValue1, (intsToInt $ replicate (length1 - length2) 0 ++ value2Digs), base)
  | length2 > length1     = radixAddition ((intsToInt $ replicate (length2 - length1) 0 ++ value2Digs), intValue2, base)
  | otherwise             = case radixAddition' value1Digs value2Digs of
                                 (0, zs) -> intsToInt zs
                                 (x, zs) -> intsToInt (x:zs)
  where
  (value1Digs, value2Digs)  = (intToInts fstValue, intToInts sndValue)
  (length1, length2) = (length value1Digs, length value2Digs)
  intValue1 = intsToInt value1Digs
  intValue2 = intsToInt value2Digs

  -- Helper Function radixAddition' (fistDigits, SecondDigits, Carry)
  radixAddition' :: [Int] -> [Int] -> (Int, [Int])
  radixAddition' [] []
    = (0, [])
  radixAddition' (x:xs) (y:ys)
    = (carry', a:as)
    where
    (carry, as) = radixAddition' xs ys
    (carry', a) = quotRem (x + y + carry) base


-- Function: radixSubtraction (firstValue, secondValue, Base)
-- Pre: all values must be positive, firstValue must be larger than secondValue, values must be in the integer set
radixSubtraction :: (Int, Int, Int) -> Int
radixSubtraction (fstValue, sndValue, base)
  = case radixSubtraction' value1Digs value2Digs' of
                                 (x, zs) -> intsToInt zs
  where
  (value1Digs, value2Digs)  = (intToInts fstValue, intToInts sndValue)
  (length1, length2) = (length value1Digs, length value2Digs)
  value2Digs'  = replicate (length1 - length2) 0 ++ value2Digs

  -- Helper Function: radixSubtraciton' (fistDigits, SecondDigits, Carry)
  radixSubtraction' :: [Int] -> [Int] -> (Int, [Int])
  radixSubtraction' [] []
    = (0, [])
  radixSubtraction' (x:xs) (y:ys)
    = (carry, z:zs)
    where
    (carry', zs) = radixSubtraction' xs ys
    carry = if (y > (x-carry')) then 1 else 0
    z = if ( y > (x - carry')) then (((x-carry') + (base)) - y) else ((x-carry')-y)






