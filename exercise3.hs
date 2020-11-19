----------------------------------------------
-- Course: DelftX FP101x
-- Exercise 3
-- Author: Paul Romeo
-- Date: November 12, 2020
----------------------------------------------
-- | Define functions
import Prelude hiding (replicate, shift, encode, find)
import Data.Char


 -- | function sumOfSquare
 --   - returns:
 --       The sum of values in the new list where
 --         each element equals to an element of the input list
 --         that is raised to the power of two.
 --   - parameters:
 --     - xs: The input list
 -- >
 --  sumOfSquare [1..5] => 55
sumOfSquare :: [Int] -> Int
sumOfSquare xs = sum [x ^ 2 | x <- xs]


-- | function replicate
--   - returns:
--      - A list with the duplicated number
--   - parameters
--      - n: The number of duplication
--      - a: The value to be duplicated
-- >
--  replicate 5 True => [True, True, True, True, True]
replicate :: Int -> a -> [a]
replicate n a = [a | _ <- [1..n]]


-- | function pyths
--   - returns:
--     - A list of tuples that contains three values x, y, and z
--        such as x^2 + y^2 = z^2 and x, y, z are in [1..n]
--   - parameters:
--     - n: input value
-- >
--  pyths 10 => [(3,4,5), (4,3,5), (6, 8, 10), (8, 6, 10)]
pyths :: Int -> [(Int, Int, Int)]
pyths n = [(x, y, z) | x <- [1..n], y <- [1..n], z <- [1..n], x^2 + y^2 == z^2]


-- | function factors
--   - returns:
--     - A list of factors where factor is a number
--        that n divide evenly by it. A factor must belong to one of the value
--        in the list created by [1..n]
--   - parameters:
--     - n: input value
-- >
--  factors 15 => [1, 3, 5, 15]
factors :: Int -> [Int]
factors n =[x | x <- [1..n], n `mod` x == 0]


-- | "perfects n" returns a list of perfect numbers. A perfect number equals
--      the sum of its factors, excluding the number itself
-- >
--  perfects 500 => [6, 28, 496]
perfects :: Int -> [Int]
perfects n = [x | x <- [1..n], isPerfect x]
  where isPerfect x = sum(init(factors x)) == x


-- | "find k t" checks each tuple (a, b) in the list t and returns a new list of
--      the second value (b) of the tuple having its first value (a) matched k
-- >
--  find 0 [(1, 0), (0, 1), (0, 2), (1, 3)] => [1, 2]
--  Notes:                  ^  |    ^  |
--    ^ : values match input k
--    | : values to be saved in the new list
find :: (Eq a) => a -> [(a, b)] -> [b]
find a t = [b | (a', b) <- t, a' == a ]


-- | "positions x xs" returns a new list having the value of elements in the input list
--      that matches x
-- >
--  positions 0 [1, 0, 0, 1, 1, 0] => [1,2,5]
positions :: (Eq a) => a -> [a] -> [Int]
positions x xs = find x (zip xs [0..n])
  where n = length xs -1


-- | "scalarproduct xs ys" returns the product of two list
-- >
--  scalarproduct [1,2,3] [4,5,6] => sum [1*4, 2*5, 3*6] = 32
scalarproduct :: [Int] -> [Int] -> Int
scalarproduct xs ys = sum [x*y | (x,y) <- xs `zip` ys]


-- | "letterToInt c" returns the integer value of the input letter c
-- >
--  letterToInt 'c' => 2
--  letterToInt 'a' => 0
letterToInt :: Char -> Int
letterToInt c = ord c - ord 'a'


-- | "IntToLetter n" returns the equivalent letter of input value n
-- >
--  IntToLetter 0 => 'a'
--  IntToLetter 2 => 'c'
intToLetter :: Int -> Char
intToLetter n = chr (ord 'a' + n)


-- | "shift n c" returns the lowercase letter after shifting by n, or
--      the same uppercase letter (shifting have no effect on uppercase letter)
-- >
--  shift 1 'a' => 'b'
--  shift 2 'a' => 'c'
--  shift 1 'A' => 'A'
shift :: Int -> Char -> Char
shift n c
  | isLower c = intToLetter ((letterToInt c + n) `mod` 26)
  | otherwise = c


-- | "encode n xs" returns the encoding message of xs where all lowercase letters
--      will be shifted by n postions and the uppercase letters will be the same
-- >
--  encode 2 "Hello" => "Hgnnq" // lowercase letters: 'e' => 'g', 'l' => 'n', 'o' => 'q'
encode :: Int -> String -> String
encode n xs = [shift n x | x <- xs]


-- | "ex3_8 xs ys" returns a list of tuples containing all possible combinations of elements from the two lists  xs and ys
-- >
--  ex3_8 [1,2] [1,2] => [(1,1), (1,2), (2,1), (2,2)]
ex3_8 :: [Int] -> [Int] -> [(Int, Int)]
ex3_8 xs ys = [(x,y) | x <- xs, y <- ys]


-- | "ex3_9 xs" creates a list of numbers where the value repeats itself by its magnitude
-- >
--  ex3_9 [1,2,3] => [1,2,2,3,3,3]
--  ex3_9 [1,2,3,4] => [1,2,2,3,3,3,4,4,4,4]
ex3_9 :: [Int] -> [Int]
ex3_9 xs = [x | x <- xs, y <- [1..x]]


-- | "sumOfEvenNumbers n" retuns the sum of all even numbers of 1 to n
-- >
--  sumOfEvenNumbers 10 => 30
sumOfEvenNumbers :: Int -> Int
sumOfEvenNumbers n = sum [x | x <- [1..n], even x]


-- | "ex3_11 xs" returns a new list of numbers with content of the input list xs and the following additios∷
--      (1) insert "1" to the beginning of the new list,
--      (2) append a number to the end of the new list having the value = last value + 1 of the input list xs
-- >
--  ex3_11 [1,2,3] => [1,2,3,4]
--  ex3_11 [3,4,5] => [1,3,4,5,6]
ex3_11 :: [Int] -> [Int]
ex3_11 xs = 1:[x+1 | x <- xs]


-- | "riffle xs ys" creates a new list of interleaved numbers from the two input lists
-- >
--  riffle [1,2,3] [4,5,6] => [1,4,2,5,3,6]
riffle :: [Int] -> [Int] -> [Int]
riffle xs ys = concat [[x,y] | (x,y) <- xs `zip` ys]


-- | "divide x y" returns True if x `divide` y with no remainder or False otherwise
-- >
--  divide 15 3 => True
--  divide 15 2 => False
divide :: Int -> Int -> Bool
divide x y
  | x `mod` y == 0 = True
  | otherwise = False


-- | "divisor n xs" returns a list of numbers from 1 to n that divides evenly to n
--
-- > E.g. divisors 15 => [1, 3, 5, 15]
divisors :: Int -> [Int]
divisors n = [x | x <- [1..n], n `divide` x]



-- | main program starts
main :: IO()
main = do
putStrLn("**********************************")

putStrLn(" # Ex 3-0: sumOfSquare [1..5] = " ++ show( sumOfSquare [1..5] ))
putStrLn(" # Ex 3-1: replicate 5 \"Hello\" = " ++ show( replicate 5 "Hello"  ))
putStrLn(" # Ex 3-2: pyths 10 = " ++ show( pyths 10  ))
putStrLn(" # Ex 3-3: perfects 500 = " ++ show( perfects 500 ))
putStrLn(" # Ex 3-4: concat [[(x,y) | y <- [4,5,6], x <- [1,2,3]]] = " ++
  show( concat [[(x,y) | y <- [4,5,6], x <- [1,2,3]]] ))
putStrLn(" # Ex 3-5: positions 0 [1, 0, 0, 1, 1, 0] = " ++ show( positions 0 [1, 0, 0, 1, 1, 0] ))
putStrLn(" # Ex 3-6: scalarproduct [1,2,3] [4,5,6] = " ++ show( scalarproduct [1,2,3] [4,5,6] ))
putStrLn(" # Ex 3-7: encode 2 \"Hello\" = " ++ show( encode 2 "Hello" ))
putStrLn(" # Ex 3-8: ex3_8 [1,2] [1,2] = " ++ show( ex3_8 [1,2] [1,2] ))
putStrLn(" # Ex 3-9: ex3_9 [1,2,3] = " ++ show( ex3_9 [1,2,3] ))
putStrLn(" # Ex 3-10: sumOfEvenNumbers 10 = " ++ show( sumOfEvenNumbers 10 ))
putStrLn(" # Ex 3-11: ex3_11 [1,2,3] = " ++ show( ex3_11 [1,2,3] ))
putStrLn(" # Ex 3-12: riffle [1,2,3] [4,5,6] = " ++ show( riffle [1,2,3] [4,5,6] ))
putStrLn(" # Ex 3-13: divisors 20 = " ++ show( divisors 20 ))

putStrLn("**********************************")
