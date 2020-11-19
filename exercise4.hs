----------------------------------------------
-- Course: DelftX FP101x
-- Exercise: 4
-- Author: Paul Romeo
-- Date: November 12, 2020
----------------------------------------------

-- Define functions --------------------------

-- | "toDigits" converts the number into a list of
--    single digits in the same order
--
-- > E.g. toDigits 12345 => [1, 2, 3, 4, 5]
toDigits :: Integer -> [Integer]
toDigits x = reverse (toDigitsRev x)

-- |"toDigitsRev" reverses a number to a list of single digits from right to left
--    (in reverse order)
--
-- > E.g. toDigitsRev 12345 => [5, 4, 3, 2, 1]
toDigitsRev :: Integer -> [Integer]
toDigitsRev x
    | x <= 0 = []
    | otherwise = (x `mod` 10) : (toDigitsRev (x `div` 10))


-- | "doubleEveryOtherFromLeft" doubles every other number in a list,
--   starting from the left. The result is a list of the same length where
--   the second, fourth, etc. numbers are doubled.
--
-- > E.g. doubleEveryOtherFromLeft [1, 2, 3, 4] => [1, 4, 3, 8]
--  ^ are digit numbers to be doubled  ^     ^         |     |
doubleEveryOtherFromLeft :: [Integer] -> [Integer]
doubleEveryOtherFromLeft [] = []
doubleEveryOtherFromLeft (x:[]) = [x]
doubleEveryOtherFromLeft (x:y:xs) = [x, 2*y] ++ doubleEveryOtherFromLeft xs


-- | "doubleEveryOtherFromRight" starting from the right. The result is
-- a list of the same length where the second-to-last, fourth-to-last, etc.
-- numbers are doubled.
--
-- > E.g. doubleEveryOtherFromRight [1, 2, 3, 4] == [2, 2, 6, 4]
--          ^ are digit to be doubled   ^     ^         |     |
doubleEveryOtherFromRight :: [Integer] -> [Integer]
doubleEveryOtherFromRight xs = reverse (doubleEveryOtherFromLeft (reverse xs))


-- | "doubleEveryOther" starting from the right. The result is
-- a list of the same length where the second-to-last, fourth-to-last, etc.
-- numbers are doubled.
--
-- > E.g. doubleEveryOther [1, 2, 3] == [1, 4, 3]
-- > E.g. doubleEveryOther [1, 2, 3, 4] == [2, 2, 6, 4]
doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther = doubleEveryOtherFromRight


-- | "sumDigits" adds up all digits in the list.
--
-- > E.g. sumDigits [1, 2, 3] => 6
-- > E.g. sumDigits [10, 11, 12] => 33
sumDigits :: [Integer] -> Integer
sumDigits [] = 0
sumDigits (x:xs) = sum (toDigits x) + sumDigits xs



-- | Description: "validate" a credit card number. Returns True if the
--    credit card number is valid, or False otherwise.
--
-- Validation Algorithm:
-- 1. Double the value of every other second digit beginning from the right.
-- 2. Add the digits of the doubled values and the undoubled digits from the
--    original number.
-- 3. Calculate the remainder when the sum is divided by 0.
-- 4. Check if the remainder is 0. If so, then the credit card number is valid.
--
-- Example:
-- 1. 1386 -> [2, 3, 16, 6]
-- 2. 2+3+1+6+6 == 18
-- 3. 18 `mod` 10 == 8
-- 4. 8 /= 0, so 1386 is not a valid credit card number.
--
-- > E.g. validate 4012888888881881 == True
-- > E.g. validate 4012888888881882 == False
validate :: Integer -> Bool
validate x = sumDigits (doubleEveryOther (toDigits x)) `mod` 10 == 0


-- | main program starts
main :: IO()
main = do

putStrLn("**********************************")

putStrLn("\n # validate 4012888888881881 : " ++ show( validate 4012888888881881 ))
putStrLn("\n # validate 4012888888881882 : " ++ show( validate 4012888888881882 ))


putStrLn("**********************************")
