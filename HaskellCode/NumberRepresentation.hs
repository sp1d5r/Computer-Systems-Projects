module NumberRepresentation where

import Data.Char


-- Where each higher order function was used:
--    - foldr1/foldr        }= used in decToBin, decToBin', decToOct, intsToInt, binToDec
--    - iterate             }= used in decToBin, decToBin', decToOct, decToHex, intsToInt, binToDec
--    - take/takeWhile      }= used in decToBin, decToBin', decToOct, decToHex, binToDec,
--    - zipWith             }= used in decToBin, decToBin', decToOct, binToDec,
--    - quotRem             }= used in decToBin, decToHex,
--    - reverse             }= used in decToBin', decToOct, decToHex, intsToInt
--    - show                }= used in intsToInt
--    - read                }= Could be used in line 80 inside the map parameter in intsToInt
--    - splitAt             }= used in binToOctal



-- Function decToBin    (Using Division)
-- Pre : input value must be a positive value
-- function takes in a decimal number (base 10) and returns the binary equivalent (base 2) using the division method:
-- step 1) divide the decimal number by 2 and record the quotient and remainder
-- step 2) repeat step 1 to the quotient until the quotient is zero
-- step 3) go through the remainders in the reverse order to get your value
decToBin :: Int -> Int
decToBin 0
  = 0

decToBin dec
  = foldr1 (+) $ zipWith (*) binaryArray ( take (length binaryArray) $ iterate (*10) 1)
  where
  binaryArray = decToBin' dec
  decToBin' :: Int -> [Int]
  decToBin' dec
   | quotient /= 0      = remainder : decToBin' quotient
   | otherwise          = remainder : []
   where
   (quotient, remainder) = quotRem dec 2


-- Function decToBin1     (Using Subtraction)
-- Pre : input must be a positive value
-- function takes in a decimal number (base 10) and returns teh binary equivalent (base 2) using the subtraction method:
-- step 1) create a list of the powers of 2 starting from the greatest power which is just less than the decimal to 1
-- step 2, a ) now the element is smaller than the decimal value replace this element with a 1, continue to step 3 with
--            the value of the decimal minus the element
-- step 2, b ) if the element is larger than teh decimal value, replace it with 0 and keep a record of your decimal value
-- step 3) repeat step 2 with the recorded value until every element in the list is complete.
-- step 4) convert your array back into an integer
decToBin1 :: Int -> Int
decToBin1 0
  = 0

decToBin1 dec
  = foldr1 (+) $ zipWith (*) binaryArray (reverse $ take (length binaryArray)  (iterate (*10) 1))
  where
  binaryArray = decToBin1' dec powersOf2
  powersOf2 = reverse $ takeWhile (<= dec) $ iterate (*2) 1
  decToBin1' :: Int -> [Int] -> [Int]
  decToBin1' dec []
    = []
  decToBin1' dec (x:xs)
    | x <= dec            = 1 : decToBin1' (dec - x) xs
    | otherwise           = 0 : decToBin1' dec xs


-- Function decToOct
-- Pre : decimal number must not be a positive value
-- This function converts a decimal value into an octal value using the subtraction method.
decToOct :: Int -> Int
decToOct 0
  = 0

decToOct dec
  = foldr1 (+) $ zipWith (*) octalArray (reverse $ take (length octalArray)  (iterate (*10) 1))
    where
    octalArray = decToOct' dec powersOf8
    powersOf8 = reverse $ takeWhile (<= dec) $ iterate (*8) 1
    decToOct' :: Int -> [Int] -> [Int]
    decToOct' dec []
      = []
    decToOct' dec (x:xs)
      | x <= dec            =  (quot dec x) : decToOct' (mod dec x) xs
      | otherwise           = 0 : decToOct' dec xs

-- Function decToHex
-- Pre : This function takes in a Decimal Number and returns a Hexadecimal Value
-- It uses the subtraction method as conducted for the previous methods.
decToHex :: Int -> [Char]
decToHex 0
  = "0"

decToHex dec
  = hexArray
    where
    integerHexArray = decToBin1' dec powersOf16
    hexArray = map (\x -> if x<10 then intToDigit x else chr (x + (ord 'A' - 10))) integerHexArray
    powersOf16 = reverse $ takeWhile (<= dec) $ iterate (*16) 1
    decToBin1' :: Int -> [Int] -> [Int]
    decToBin1' dec []
      = []
    decToBin1' dec (x:xs)
      | x <= dec            =  (quot dec x) : decToBin1' (mod dec x) xs
      | otherwise           = 0 : decToBin1' dec xs

-- Function intToInts
-- Pre : Cannot be a negative value
-- Takes an integer and returns an array of it's digits
-- Alternative Solution using Data.Char is shown line 60
intToInts :: Int -> [Int]
intToInts 0
  = [0]
intToInts value
  = intToInts (div value 10) ++ [mod value 10]

intToInts' :: Int -> [Int]
intToInts' value
  = map (digitToInt) $ show value


-- Function intsToInt
-- Pre : each integer in the array of integers must be a positive value
intsToInt :: [Int] -> Int
intsToInt []
  = 0
intsToInt value
  | foldr1 (+) value == 0     = 0
  | otherwise                 = foldr (+) 0 $ zipWith (*) value $ reverse $ take (length value) $ iterate (*10) 1


-- Function binToDen
-- Pre : Cannot be a negative value
-- This function converts binary integers into Decimal integers
binToDec :: Int -> Int
binToDec value
  = foldr1 (+) $ zipWith (*) arrayValue powOf2
  where
  arrayValue = intToInts value
  powOf2 = reverse $ take (length arrayValue) $ iterate (*2) 1


-- Function binToOct
-- Pre : Cannot be a negative value
-- This function converts a binary value (base 2) into an octal value (base 8).
binToOct :: Int -> Int
binToOct binValue
  = intsToInt $ binToOct' binArray
  where
  binArray = intToInts binValue
  binToOct' :: [Int] -> [Int]
  binToOct' binValue
    | (length binValue) >= 3       = binToOct' firstPart  ++ [binToDec $ intsToInt lastThree]
    | otherwise                    = [binToDec $ intsToInt binValue]
    where
    (firstPart, lastThree) = splitAt ((length binValue) - 3) binValue
