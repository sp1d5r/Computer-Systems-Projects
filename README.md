# Computer System Projects

-- This is built for the students at Imperial College London -- 

A series of programs written in Haskell and Java to help better your understanding of the intro to computer systems 
module (112) as well as your Haskell and Java modules (120.1 and 120.2 respectively). 

To improve your understanding of Haskell lots of higher order functions will be used, I encourage you to try to 
understand how each function is working.

## Table of Contents
* [Topic 1: Number Representation](#number-representation)
* [Topic 2: Radix Arithmetic](#radix-arithmetic)
* [Topic 3: Floating Point Representation](#floating-point-representation)
* [Topic 4: Boolean Algebra, Gates, IC's, and Boolean Functions](#boolean-algebra-gates-ic's-and-boolean-functions)
* [Topic 5: Canonical Forms and Karnaugh Maps](#canonical-forms-and-karnaugh-maps)
* [Topic 6: Combinatorial Circuits](#combinatorial-circuits)
* [Topic 7: Time Dependant Behaviour of Digital Circuits and Feedback](#time-dependant-behaviour-of-digital-circuits-and-feedback)
* [Topic 8: Sequential Circuits and Flip Flops](#sequential-circuits-and-flip-flops)
* [Topic 10: Finite State Representation of Digital Circuits](#finite-state-representation-of-digital-circuits)
* [Topic 11: Traffic Lights (Example)](#traffic-lights-(example))
* [Topic 12: Registers](#registers)
* [Topic 13: Multiplexers, Decoders, Comparators etc](#multiplexers-decoders-comparators-etc)
* [Topic 14: Arithmetic](#arithmetic)
* [Topic 15 Manual Processors](#manual-processors)
* [Topic 16: 32 Bit Computer Architecture](#32-bit-computer-architecture)
* [Topic 16: 32 Bit Computer Sequencing](#32-bit-computer-sequencing)
* [Additional Resources](#additional-resources)


# Number Representation 

Unit 1 of this module, is all about converting the bases (radices) of number systems, i.e. Binary to Decimal /Base 2 to 
Base 10. Check the code source files to see how you would program these while incorporating higher order functions. 
* [Haskell Source Code](HaskellCode/NumberRepresentation.hs)
* [Java Source Code](JavaCode/NumberRepresentation.java)

Haskell Code for Converting decimal numbers into binary numbers using Division (line 25) :  
```haskell
decToBin :: Int -> Int
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
```

Haskell Code for Converting decimal numbers into binary numbers using Subtraction (line 50) : 
```haskell
decToBin1 :: Int -> Int
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
```

Haskell Code for Converting decimal numbers into octal numbers (line 70): 
```haskell
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
```

Haskell Code for converting decimal numbers into hexadecimal values (line 89):
```haskell
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
```

Haskell Code for converting binary numbers into decimal numbers (line 134):
```haskell
binToDec :: Int -> Int
binToDec value
  = foldr1 (+) $ zipWith (*) arrayValue powOf2
  where
  arrayValue = intToInts value
  powOf2 = reverse $ take (length arrayValue) $ iterate (*2) 1
``` 

Haskell Code for converting binary numbers into octal numbers (line 145):
```haskell
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
```

Java Code for Converting using Division (line x):
**Under Construction** TODO:// finsih off code for this section.


# Radix Arithmetic 
Unit 2 of this module is about radix arithmetic, this involves radix addition, subtraction, Unlike the code for the 
topic 1 this will not focus on particular bases instead you will be allowed to input the required base you would like. 
Please see the code for more detail.

Check the code source files to see how you would program these while incorporating higher order functions.
* [Haskell Source Code](HaskellCode/RadixArithmetic.hs)
* [Java Source Code](JavaCode/RadixArithmetic.java) TODO:// add file 

Haskell code for radix Arithmetic (line x):
```haskell
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

  -- Helper Function radixAddition' [firstList] [SecondList]
  radixAddition' :: [Int] -> [Int] -> (Int, [Int])
  radixAddition' [] []
    = (0, [])
  radixAddition' (x:xs) (y:ys)
    = (carry', a:as)
    where
    (carry, as) = radixAddition' xs ys
    (carry', a) = quotRem (x + y + carry) base
```

Haskell code for radix subtraction (line x):
```haskell
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
```

# Floating Point Representation
TODO:// Complete this section and other sections. 

# Additional Resources
TODO :// add extra resources.
- link to computer systems books

# Social Media
- [Linkden - Elijah Ahmad](https://www.linkedin.com/in/elijah-ahmad-658a2b199/)
- [FaceBook - Elijah Ahmad](https://www.facebook.com/elijah.ahmad.71)
- [Instagram - @ElijahAhmad__](https://www.instagram.com/ElijahAhmad__)
- [Snapchat - @Elijah.Ahmad](https://www.snapchat.com/add/elijah.ahmad)
