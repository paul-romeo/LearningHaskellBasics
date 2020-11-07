-- | Define functions
import Prelude hiding (pairs, sorted, positions, lowers)
import Data.Char(isUpper, isLower, isDigit)

-- pairs :: [a] -> [(a, b)]
pairs xs = zip xs (tail xs)

sorted :: Ord a => [a] -> Bool
sorted xs = and [x < y | (x, y) <- pairs xs]

-- positions :: a -> [a] -> [a]
positions x xs = [i | (x', i) <- zip xs [0..n], x == x']
   where n = length xs -1

lowers :: String -> Int
lowers xs = length [x | x <- xs, isLower x]

-- factors n builds a list of numbers of n divide evenly to the number in the list1
factors :: Int -> [Int]
factors n =[x | x <- [1..n], n `mod` x == 0]



-- | main program starts
main :: IO()
main = do

putStrLn("**********************************")
putStrLn("\n # zip ['a', 'b', 'c'] [1, 2, 3, 4] = " ++ show(zip ['a', 'b', 'c'] [1, 2, 3, 4]))
putStrLn("\n # pairs [1, 2, 3, 4] = " ++ show( pairs [1, 2, 3, 4] ))
putStrLn("\n # pairs ['a', 'b', 'c', 'd'] = " ++ show( pairs ['a', 'b', 'c', 'd'] ))
putStrLn("\n # sorted [1, 2, 3, 4] = " ++ show( sorted [1, 2, 3, 4] ))
putStrLn("\n # sorted (reverse ['d', 'c', 'b', 'a']) = " ++ show( sorted (reverse ['d', 'c', 'b', 'a'] )))
putStrLn("\n # positions 0 [1, 0, 0, 1, 1, 0, 1, 0] = " ++ show( positions 0 [1, 0, 0, 1, 1, 0, 1, 0] ))
putStrLn("\n # length \"abcde\" = " ++ show( length "abcde" ))
putStrLn("\n # take 3 \"abcde\" = " ++ show( take 3 "abcde" ))
putStrLn("\n # zip \"abcde\" [1, 2, 3] = " ++ show( zip "abcde" [1, 2, 3] ))
putStrLn("\n # lowers \"PaulRomeo\" = " ++ show( lowers "PaulRomeo" ))

putStrLn("**********************************")
