-- | Define functions
import Prelude hiding (map, filter)
import Data.Char 

-- | high-order function map
--   - returns:
--     - A new list of values results from mapping the input list xs based on function f
--   - parameters:
--     - f: function to be specified
--     - xs: list of values
-- >
--  map (1+) [1,3,5,7] => [2,4,6,8]
--  Note: (a -> b) => function () processes value a and returns value b
map :: (a->b) -> [a] -> [b]
map f xs = [f x | x <- xs]

-- | high-order function map' implements using recursion
--   - returns:
--     - A new list of values results from mapping' the input list xs based on function f
--   - parameters:
--     - f: function to be specified
--     - (x:xs): list of values
-- >
--  map (1+) [1,3,5,7] => [2,4,6,8]
map' :: (a->b) -> [a] -> [b]
map' f [] = []
map' f (x:xs) = f x : map' f xs


-- | high-order function filter
--   - returns:
--     - A new list of values results from filtering the input list xs based on function f
--   - parameters:
--     - f: function to be specified
--     - xs: list of values
-- >
--  filter even x [1..10] => [2,4,6,8,10]
--  Note: (a -> Bool) => function () processes value a and return a Boolean value
filter :: (a -> Bool) -> [a] -> [a]
filter f xs = [x | x <- xs, f x]


-- | high-order function filter' implements using recursion
--   - returns:
--     - A new list of values results from filtering the input list (x:xs) based on function f
--   - parameters:
--     - f: function to be specified
--     - xs: list of values
-- >
--  filter' even x [1..10] => [2,4,6,8,10]
filter' :: (a -> Bool) -> [a] -> [a]
filter' f [] = [] -- when list is now empty, exit the function and return the result list
filter' f (x:xs)
  | f x = x: filter' f xs  -- f x is True: add value x to the result list and filter on the remaining list
  | otherwise = filter' f xs -- otherwise: filter on the remaining list


  -- | high-order function twice
  --   - returns:
  --     - A new value based on executing function f two times
  --   - parameters:
  --     - f: function to be specified
  --     - x: integer value
  -- >
  --  twice (2+) 3 => 2+ (2+3) = 7
twice :: (a -> a) -> a -> a
twice f x = f (f x)


-- | function foldr'
--    The high-order base function for use to implement other functions
foldr' :: (a -> b -> b) -> b -> [a] -> b
foldr' f v [] = v
foldr' f v (x:xs) = f x (foldr' f v xs)


-- | function sum'
--   - returns:
--     - The sum of all values in the input list (x:xs)
--   - parameters:
--     - (x:xs): The input list
-- >
--  sum' [1,2,3] => 5
sum' :: [Int] -> Int
sum' [] = 0
sum' (x:xs) = x + sum' xs


-- | function sum'' tmplements using base-function foldr'
--   - returns:
--     - The sum of all values in the input list (x:xs)
--   - parameters:
--     - (x:xs): The input list
-- >
--  sum'' [1,2,3] => 5
sum'' :: [Int] -> Int
sum'' (x:xs)= foldr' (+) 0 (x:xs)


-- | function product'
--   - returns:
--     - The product of all values in the input list (x:xs)
--   - parameters:
--     - (x:xs): The input list
-- >
--  product' [1,2,3] => 6
product' :: [Int] -> Int
product' [] = 1
product' (x:xs) = x * product' xs


-- | function product'' implements using high-order base function foldr'
--   - returns:
--     - The product of all values in the input list (x:xs)
--   - parameters:
--     - (x:xs): The input list
-- >
--  product'' [1,2,3] => 6
product'' :: [Int] -> Int
product'' (x:xs) = foldr' (*) 1 (x:xs)

-- | function and'
--   - returns:
--     - Logical AND for all boolean values in the input list (x:xs)
--   - parameters:
--     - (x:xs): The input list
-- >
--  and' [True, False, True] => False
and' :: [Bool] -> Bool
and' [] = True
and' (x:xs) = x && and' xs



-- | function and'' implemented using the foldr' base-function 
--   - returns:
--     - Logical AND for all boolean values in the input list (x:xs)
--   - parameters:
--     - (x:xs): The input list
-- >
--  and'' [True, False, True] => False
and'' :: [Bool] -> Bool
and'' (x:xs) = foldr' (&&) True (x:xs)



-- | function or'
--   - returns:
--     - Logical OR for all boolean values in the input list (x:xs)
--   - parameters:
--     - (x:xs): The input list
-- >
--  or' [True, False, True] => True
--  or' [False, False, False] => False
or' :: [Bool] -> Bool
or' [] = False
or' (x:xs) = x || or' xs



-- | function or'' implemented using the foldr' base-function 
--   - returns:
--     - Logical AND for all boolean values in the input list (x:xs)
--   - parameters:
--     - (x:xs): The input list
-- >
--  or' [True, False, True] => True
--  or' [False, False, False] => False
or'' :: [Bool] -> Bool
or'' (x:xs) = foldr' (||) False (x:xs)



-- | function reverse'
--   - returns:
--     - A newlist containing all elements of the input list (x:xs) in reverse order
--   - parameters:
--     - (x:xs): The input list
-- >
--  reverse' [1,2,3] => [3,2,1]
reverse' :: [a] -> [a]
reverse' [] = []
reverse' (x:xs) = reverse' xs ++ [x]


-- | function reverse'' implemented using base-function foldr'
--   - returns:
--     - A newlist containing all elements of the input list (x:xs) in reverse order
--   - parameters:
--     - (x:xs): The input list
-- >
--  reverse'' [1,2,3] => [3,2,1]
reverse'' :: [a] -> [a]
reverse'' (x:xs) = foldr' (\ x xs -> xs ++ [x]) [] (x:xs)



-- | function length'
--   - returns:
--     - A number represent the number of elements in the input list (x:xs)
--   - parameters:
--     - (x:xs): The input list
-- >
--  length' [1,2,3] => 3
length' :: [a] -> Int
length' [] = 0
length' (_:xs) = 1+ length' xs


-- | function length'' implemented using base-function foldr'
--   - returns:
--     - A number represent the number of elements in the input list (x:xs)
--   - parameters:
--     - (x:xs): The input list
-- >
--  length'' [1,2,3] => 3
length'' :: Num n => [a] -> n
length'' [] = 0
length'' (_:xs) = foldr' (const (1+)) 1 xs

-- | Other library functions 


-- | function (.) 
--   - returns:
--     - A single function as the composition of two functions 
--   - parameters:
--     - f: function 1 
--     - g: function 2 
-- >
(.) :: (b -> c) -> (a -> b) -> (a -> c)
f . g  = \x -> f (g x)

-- | function all' 
--   - returns:
--     - Boolean True when the evaluating function of all values in the input list xs are True, 
--        or False otherwise
--   - parameters:
--     - p: evaluating function  
--     - xs: the input list  
-- >
--   all' even [2, 4, 6, 8] -> True 
--   all' even [2, 4, 6, 7] -> False 
all' :: (a -> Bool) -> [a] -> Bool 
all' p xs = and [p x | x <- xs]



-- | function any' 
--   - returns:
--     - Boolean True when the evaluating function of any values in the input list xs is True, 
--        or False otherwise
--   - parameters:
--     - p: evaluating function  
--     - xs: the input list  
-- >
--   any' odd [2, 4, 6, 8] -> False 
--   any' odd [2, 4, 6, 7] -> True 
any' :: (a -> Bool) -> [a] -> Bool 
any' p xs = or [p x | x <- xs]


-- | function takeWhile' 
--   - returns:
--     - list/string containing values before the evaluating function fails 
--   - parameters:
--     - p: evaluating function  
--     - xs: the input list  
-- >
--   takeWhile' isAlpha "Hello World!"  => "Hello"
takeWhile' :: (a -> Bool) -> [a] -> [a]
takeWhile' p [] = []
takeWhile' p (x:xs) 
  | p x = x : takeWhile' p xs 
  | otherwise = []



-- | function dropWhile' 
--   - returns:
--     - list/string containing values AFTER the evaluating function fails 
--   - parameters:
--     - p: evaluating function  
--     - xs: the input list  
-- >
--   dropWhile' isSpace "   Hello"  => "Hello" // delete the leading blank space
dropWhile' :: (a -> Bool) -> [a] -> [a]
dropWhile' p [] = []
dropWhile' p (x:xs) 
  | p x = dropWhile' p xs 
  | otherwise = x:xs


-- | main program starts
main :: IO()
main = do

putStrLn("**********************************")

putStrLn("\n # twice (2+) 3 = " ++ show( twice (2+) 3 ))

putStrLn("\n # map (1+) [1,3,5,7] = " ++ show( map (1+) [1, 3, 5, 7] ))
putStrLn(" # map' (1+) [1,3,5,7] = " ++ show( map' (1+) [1, 3, 5, 7] ))

putStrLn("\n # filter odd [1..10] = " ++ show( filter odd [1..10] ))
putStrLn(" # filter' even [1..10] = " ++ show( filter' even [1..10] ))

putStrLn("\n # sum' [1,2,3] = " ++ show( sum' [1,2,3] ))
putStrLn(" # sum'' [1,2,3] = " ++ show( sum'' [1,2,3] ))

putStrLn("\n # product' [1,2,3] = " ++ show( product' [1,2,3] ))
putStrLn(" # product'' [1,2,3] = " ++ show( product'' [1,2,3] ))

putStrLn("\n # and' [True,False,True] = " ++ show( and' [True,False,True] ))
putStrLn(" # and' [True,True,True] = " ++ show( and' [True,True,True] ))
putStrLn(" # and'' [True,False,True] = " ++ show( and'' [True,False,True] ))
putStrLn(" # and'' [True,True,True] = " ++ show( and'' [True,True,True] ))

putStrLn("\n # or' [True,False,True] = " ++ show( or' [True,False,True] ))
putStrLn(" # or' [False,False,False] = " ++ show( or' [False,False,False] ))
putStrLn(" # or'' [True,False,True] = " ++ show( or'' [True,False,True] ))
putStrLn(" # or'' [False,False,False] = " ++ show( or'' [False,False,False] ))

putStrLn("\n # reverse' [1,2,3] = " ++ show( reverse' [1,2,3] ))
putStrLn(" # reverse'' [1,2,3] = " ++ show( reverse'' [1,2,3] ))

putStrLn("\n # length' [1,2,3] = " ++ show( length' [1,2,3] ))
putStrLn(" # length'' [1,2,3,4,5,6] = " ++ show( length'' [1,2,3,4,5,6] ))

putStrLn("\n # all' even [2, 4, 6, 8] = " ++ show( all' even [2, 4, 6, 8] ))
putStrLn(" # all' even [2, 4, 6, 7] = " ++ show( all' even [2, 4, 6, 7] ))

putStrLn("\n # any' odd [2, 4, 6, 8] = " ++ show( any' odd [2, 4, 6, 8] ))
putStrLn(" # any' odd [2, 4, 6, 7] = " ++ show( any' odd [2, 4, 6, 7] ))

putStrLn("\n # takeWhile' isAlpha \"Hello World!\"  = " ++ show( takeWhile' isAlpha "Hello World!" ))

putStrLn("\n # dropWhile' isSpace \"   Hello World!\"  = " ++ show( dropWhile' isSpace "   Hello World!" ))

putStrLn("**********************************")
