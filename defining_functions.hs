-- | Define functions

-- Conditional expression
abs_ce :: Int -> Int
abs_ce n = if n >= 0 then n else -n

signum_ce :: Int -> Int
signum_ce n = if n < 0 then -1 else if n == 0 then 0 else 1

-- Guarded equation
abs_ge :: Int -> Int
abs_ge n  | n >= 0 = n
          | otherwise = -n
signum_ge :: Int -> Int
signum_ge n | n < 0 = -1
            | n == 0 = 0
            | otherwise = 1

xor' :: Bool -> Bool -> Bool
xor' x y | x == True && y == False = True
         | x == False && y == True = True
         | otherwise = False

-- Pattern matching
not' :: Bool -> Bool
not' False = True
not' True = False

(&&&) :: Bool -> Bool -> Bool
True &&& b = b
False &&& b = False

(|||) :: Bool -> Bool -> Bool
False ||| False = False
_ ||| _ = True


(////) :: Bool -> Bool -> Bool
True //// False = True
False //// True = True
_     //// _    = False


-- List pattern (:) cons; x:xs must be enclosed in ()
-- _ means anything or don't care
-- [1, 2, 3, 4] is 1:(2: (3: 4: []))
head' :: [a] -> a
head' (x: _) = x

tail' :: [a] -> [a]
tail' (_: xs) = xs




-- | main program starts
main :: IO()
main = do

-- Display outputs : -- "show" convert value to string
putStrLn("**********************************")
putStrLn("\n abs_ce 3 = " ++ show(abs_ce 3))
putStrLn(" abs_ce 0 = " ++ show(abs_ce 0))
putStrLn(" abs_ce (-3) = " ++ show(abs_ce (-3)))

putStrLn("\n abs_ge 3 = " ++ show(abs_ge 3))
putStrLn(" abs_ge 0 = " ++ show(abs_ge 0))
putStrLn(" abs_ge (-3) = " ++ show(abs_ge (-3)))

putStrLn("\n signum_ce (-3) = " ++ show(signum_ce (-3)))
putStrLn(" signum_ce 0 = " ++ show(signum_ce 0))
putStrLn(" signum_ce 3 = " ++ show(signum_ce 3))

putStrLn("\n signum_ge (-3) = " ++ show(signum_ge (-3)))
putStrLn(" signum_ge 0 = " ++ show(signum_ge 0))
putStrLn(" signum_ge 3 = " ++ show(signum_ge 3))

putStrLn("\n not True = " ++ show(not True))
putStrLn(" not False = " ++ show(not False))

putStrLn("\nAND operation: ")
putStrLn(" True &&& True = " ++ show(True &&& True))
putStrLn(" True &&& False = " ++ show(True &&& False))
putStrLn(" False &&& True = " ++ show(False &&& True))
putStrLn(" False &&& False = " ++ show(False &&& False))

putStrLn("\nOR operation:")
putStrLn(" True ||| True = " ++ show(True ||| True))
putStrLn(" True ||| False = " ++ show(True ||| False))
putStrLn(" False ||| True = " ++ show(False ||| True))
putStrLn(" False ||| False = " ++ show(False ||| False))

putStrLn("\nxor' True True = " ++ show(xor' True True))
putStrLn(" xor' True False = " ++ show(xor' True False))
putStrLn(" xor' False True = " ++ show(xor' False True))
putStrLn(" xor' False False = " ++ show(xor' False False))

putStrLn("\nXOR operation:")
putStrLn(" True //// True = " ++ show(True //// True))
putStrLn(" True //// False = " ++ show(True //// False))
putStrLn(" False //// True = " ++ show(False //// True))
putStrLn(" False //// False = " ++ show(False //// False))

putStrLn("\n head' [1, 2, 3, 4] = " ++ show(head' [1, 2, 3, 4]))
putStrLn("\n tail' [1, 2, 3, 4] = " ++ show(tail' [1, 2, 3, 4]))


putStrLn("\n**********************************")
