-- | Define functions

-- (problem 1)
n = a `div` (length xs)
  where
    a = 10
    xs = [1, 2, 3, 4, 5]

-- function last returns the last element of a non-empty list
last'::[Int] -> Int   -- signature
last' xs = head(drop (length xs-1) xs)

ys = head( drop (length ys - 1) ys)
  where ys = [1, 2, 3, 4, 5]

as = [1, 2, 3, 4, 5]

bs = bs !! 4
  where bs = [1, 2, 3, 4, 5]

cs = drop (length cs) cs
  where cs = [1, 2, 3, 4, 5]

-- function double
double:: Int -> Int
double i = i + i







-- | main program start
main :: IO()
main = do
  print(double 2)

  putStrLn . show $ n
  putStrLn "last element"
  print (last' [1, 2, 3, 4, 5])

  putStrLn "last ys"
  print (ys)

  -- print (last as)

  print (bs)

  print (as !! 2)


  putStrLn "Double Double "
  print(double (double 2))
