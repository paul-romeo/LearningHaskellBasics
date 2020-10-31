-- | Define functions:
-- Function add in tradition function
add ::(Int, Int) -> Int
add(x, y) = x + y

-- Function add' in curried function
add' ::Int -> Int -> Int
add' x y = x + y

-- function multiply in curried function
multiply :: Int -> Int -> Int -> Int
multiply x y z = x * y * z

-- function zeroto
zeroto:: Int -> [Int]
zeroto n = [0..n]

-- function double
double:: Int -> Int
double i = i + i


-- | Main program starts:
main = do

putStr "\n\n[The lab is on curried functions]\n"
-- add 2 numbers using the regular function
print(add (2, 3))

-- add 2 numbers using curried function
print(add' 2 3)

-- multiply with curried function
print(multiply 2 3 4)

print(drop 5 (zeroto 10))
print(reverse (zeroto 5))
print(sum (take (double 3) (zeroto 10)))
