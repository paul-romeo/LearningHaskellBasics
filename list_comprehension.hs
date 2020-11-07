-- | Define functions
import Prelude hiding (concat)
-- list1 creates a list with values from 1 to n
list1 :: Int -> [Int]
list1 n = [x  | x <- [1..n]]

-- list2 creates a list of tuples
list2 :: [Int] -> [Int] -> [(Int, Int)]
list2 xs ys = [(x, y) | x <- xs, y <- ys]

-- list3 creates a conditional list
list3 :: [Int] -> Int -> [(Int, Int)]
list3 xs n = [(x, y) | x <- xs, y <- [x..n]]

-- list4 creates a list that extracts only even numbers from an existing list
list4 :: [Int] -> [Int]
list4 xs = [x | x <- xs, even x]

-- factors n builds a list of numbers of n divide evenly to the number in the list1
factors :: Int -> [Int]
factors n =[x | x <- [1..n], n `mod` x == 0]

-- prime n returns True when factors n == [1, n] or False otherwise
prime :: Int -> Bool
prime n = factors n == [1, n]

-- primes n creates a list of numbers that are primed
primes :: Int -> [Int]
primes n = [x | x <- [1..n], prime x]

-- concat flats out inner lists into one list
concat :: [[Int]] -> [Int]
concat xss = [x | xs <- xss, x <- xs ]

-- | main program starts
main :: IO()
main = do

putStrLn("**********************************")
putStrLn("\n list1 5 = " ++ show(list1 5))
putStrLn("\n list2 [1, 2, 3] [4, 5] = " ++ show(list2 [1, 2, 3] [4, 5]))
putStrLn("\n list3 [1, 2, 3] 3 = " ++ show(list3 [1, 2, 3] 3))
putStrLn("\n list4 [1..10] = " ++ show(list4 [1..10]))
putStrLn("\n factors 7 = " ++ show(factors 7))
putStrLn("\n factors 15 = " ++ show(factors 15))
putStrLn("\n prime 7 = " ++ show(prime 7))
putStrLn("\n prime 15 = " ++ show(prime 15))
putStrLn("\n primes 40 = " ++ show(primes 40))
putStrLn("\n concat' [[1, 2, 3], [4, 5], [6]] = " ++ show(concat [[1, 2, 3], [4, 5], [6]] ))
putStrLn("**********************************")
