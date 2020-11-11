-- | recursive-part1.hs
import Prelude hiding (factorial, product, length, reverse, zip, drop, (++))

-- | Define functions in recursive way

-- function factorial
factorial :: Integer -> Integer
factorial 0 = 1
factorial n = n * factorial(n-1)

-- function product
product :: [Integer] -> Integer
product [] = 1
product (n:ns) = n * product(ns)

-- function length
length :: [a] -> Int
length [] = 0
length (x: xs) = 1 + length xs

-- function reverse
reverse :: [a] -> [a]
reverse [] = []
reverse (x:xs) = reverse xs ++ [x]

-- function zip
zip :: [a] -> [b] -> [(a, b)]
zip [] _ = []
zip _ [] = []
zip (x:xs) (y:ys) = ((x, y) : zip xs ys)

-- function drop
drop :: Int -> [a] -> [a]
drop 0 xs = xs
drop _ [] = []
drop n (x:xs) = drop (n-1) xs

-- function (++)
(++) :: [a] -> [a] -> [a]
[] ++ ys = ys
(x:xs) ++ ys = (x:(xs ++ ys))

-- function qsort
qsort :: [Int] -> [Int]
qsort [] = []
qsort (x:xs) = qsort smallers ++ [x] ++ biggers
  where smallers = [a | a <- xs, a <= x ]
        biggers = [b | b <- xs, b > x]


-- | main program starts
main :: IO()
main = do

putStrLn("**********************************")

putStrLn("\n # factorial 5 = " ++ show( factorial 120 ))
putStrLn("\n # product [1, 2, 3, 4] = " ++ show( product [1, 2, 3, 4] ))
putStrLn("\n # length \"abcde\" = " ++ show( length "abcde" ))
putStrLn("\n # length \"\" = " ++ show( length "" ))
putStrLn("\n # length [] = " ++ show( length [] ))
putStrLn("\n # length [1, 2, 4] = " ++ show( length [1, 2, 4] ))
putStrLn("\n # reverse \"abcde\" = " ++ show( reverse "abcde" ))
putStrLn("\n # reverse [5, 4, 3, 2, 1] = " ++ show( reverse [5, 4, 3, 2, 1] ))
putStrLn("\n # zip ['a', 'b', 'c'] [4, 5, 6] = " ++ show( zip ['a', 'b', 'c'] [4, 5, 6] ))
putStrLn("\n # drop 2 [1, 2, 3, 4] = " ++ show( drop 2 [1, 2, 3, 4]  ))
putStrLn("\n # [1, 2, 3] ++ [4, 5, 6] = " ++ show( [1, 2, 3] ++ [4, 5, 6] ))
putStrLn("\n # qsort [3, 2, 4, 1, 5] = " ++ show( qsort [3, 2, 4, 1, 5] ))

putStrLn("**********************************")
