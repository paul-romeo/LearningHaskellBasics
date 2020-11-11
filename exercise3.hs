
-- | Define functions
import Data.Char

factors :: Int -> [Int]
factors n =[x | x <- [1..n], n `mod` x == 0]

perfects :: Int -> [Int]
perfects n = [x | x <- [1..n], isPerfect x]
  where isPerfect x = sum(init(factors x)) == x

test :: [Int] -> [Int] -> [(Int, Int)]
test xs ys = [(x, y) | x <- xs, y <- ys]

scalar' :: [Int] -> [Int] -> [Int]
scalar' xs ys = [x * y | x <- xs, y <- ys]

ex9 xs = [x | x <- xs, y <- [1..x]]

divide :: Int -> Int -> Bool
divide x y = if x `mod` y == 0 then True else False

-- | main program starts
main :: IO()
main = do-- factors n builds a list of numbers of n divide evenly to the number in the list1

putStrLn("**********************************")

putStrLn("\n # perfects 500 = " ++ show( perfects 500 ))


putStrLn("\n # <text> = " ++ show( test [1, 2, 3] [4, 5, 6] ))
putStrLn("\n # scalar' [1, 2, 3] [4, 5, 6] = " ++ show( scalar' [1, 2, 3] [4, 5, 6] ))
putStrLn("\n # ex9 [1, 2, 3] = " ++ show( ex9 [1, 2, 3] ))
putStrLn("\n # divide 15 2 = " ++ show( divide 15 3 ))
putStrLn("\n # ord 'a' = " ++ show( ord 'b' ))
putStrLn("\n # ord 'a' = " ++ show( 99 `mod` 26 ))

putStrLn("**********************************")
