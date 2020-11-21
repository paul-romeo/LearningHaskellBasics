-- | Define functions

-- | function evens 
--   - returns:
--     - A new list extracts only even numbers from the input list xs 
--   - parameters:
--     - xs: the input list  
-- >
--   evens [2, 5, 6, 13, 32]  => [2,6,32]
evens :: [Int] -> [Int]
evens xs = [x | x <- xs, even x]


-- | function evens' implemented using recursion  
--   - returns:
--     - A new list extracts only even numbers from the input list xs 
--   - parameters:
--     - xs: the input list  
-- >
--   evens [2, 5, 6, 13, 32]  => [2,6,32]
evens' :: [Int] -> [Int]
evens' [] = []
evens' (x:xs) 
    | even x = x: evens' xs 
    | otherwise = evens' xs 


-- | function squares  
--   - returns:
--     - A list of values in which each value is a square of a positive number of the created list 
--   - parameters:
--     - n: an integer number >= 0  
-- >
--   squares 5 => [1*1, 2*2, 3*3, 4*4, 5*5]  => [1,4,9,16,25]
squares :: Integer -> [Integer]
squares n = [x*x | x <- [1..n]]


-- | function sumSquares  
--   - returns:
--     - The sum of squares n  
--   - parameters:
--     - n: an integer number >= 0  
-- >
--   sumSquares 5 => 55
sumSquares :: Integer -> Integer 
sumSquares n = (sum . squares) n 


-- | function squares'  
--   - returns:
--     - A list of squared number where: 
--         - the upper bound of the number equals m+n, and
--         - the lower bound of the number is n+1
--   - parameters:
--     - m: integer number >= 0
--     - n: integer number >= 0  
-- >
--   squares' x y: squares' 4 2 = [3*3, 4*4, 5*5, 6*6]
--   squares' x y: squares' 2 0 = [1*1, 2*2]
squares' :: Integer -> Integer -> [Integer]
squares' m n = [x*x | x <- [n+1 .. m+n]]



-- | function sumSquares'  
--   - returns:
--     - The sum of squares' x 
--   - parameters:
--     - n: an integer number >= 0  
-- >
--   sumSquares' 2 => sum . [3*3, 4*4] = 25
--   sumSquares' 3 => sum . [4*4, 5*5, 6*6] = 77
sumSquares' :: Integer -> Integer 
sumSquares' x = sum . uncurry squares' $ (x, x)


-- | function coords   
--   - returns:
--     - A list of tuples with a combinations of the two lists created by input m and n
--   - parameters:
--     - m: an integer number >= 0  
--     - n: an integer number >= 0  
-- >
--   coords 1 1 => ([0,0], [0,1], [1,0], [1,1])
--   coords 1 2 => ([0,0], [0,1], [0,2], [1,0], [1,1], [1,2])
coords :: Int -> Int ->[(Int, Int)]
coords m n = [(x,y) | x <- [0..m], y <- [0..n]]

-- foldr (-) 0 . map (uncurry (*)) $ coords 5 7



-- | main program starts
main :: IO()
main = do

putStrLn("**********************************")

putStrLn("\n 5-0 sum . evens' $ [827305 .. 927104] =  " ++ show(  sum . evens' $ [827305 .. 927104] ))
putStrLn(" 5-1 sum . evens' $ [] =  " ++ show(  sum . evens' $ [] ))
putStrLn(" 5-2 sum . evens' $ [1,3..] =  (can't be computed) " )
putStrLn(" 5-3 type class of squares 4 = [1*1, 2*2, 3*3, 4*4] is Integer -> [Integer]" )
putStrLn(" 5-4 sumSquares 50 = " ++ show(  sumSquares 50 ))
putStrLn(" 5-5 sumSquares' 50 = " ++ show(  sumSquares' 50 ))
putStrLn(" 5-6 sum $ squares' 10 0 = " ++ show(  sum $ squares' 10 0 ))
putStrLn(" 5-7 sum $ squares' 0 10 = " ++ show(  sum $ squares' 0 10 ))
putStrLn(" 5-8 foldr (-) 0 . map (uncurry (*)) $ coords 5 7 = " ++ show(  foldr (-) 0 . map (uncurry (*)) $ coords 5 7 ))
putStrLn(" 5-9 a f b g c = ((a f) b) g c" ) -- process from left to right 

putStrLn("\n**********************************")
