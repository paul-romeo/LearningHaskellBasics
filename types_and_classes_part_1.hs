-- | Define functions
double x = x + x
quadruple x = double (double x)
factorial n = product [1..n]
average ns = sum ns `div` length ns

-- | main
main = do
  putStr "\n\nThe first item in [1, 2, 3, 4, 5]: "
  print(head [1, 2, 3, 4, 5])

  putStr "\nAll except for the first item in [1, 2, 3, 4, 5]: "
  print(tail [1, 2, 3, 4, 5])

  putStr "\nThe list that contains three items in [1, 2, 3, 4, 5]:"
  print(take 3 [1, 2, 3, 4, 5])

  putStr "\nItem 3 of [1, 2, 3, 4, 5]: "
  print([1, 2, 3, 4, 5] !! 2)

  putStr "\nThe list that contains items 4 & 5 of [1, 2, 3, 4, 5]: "
  print(drop 3 [1, 2, 3, 4, 5])

  putStr "\nNumber of items in [1, 2, 3, 4, 5]: "
  print(length [1, 2, 3, 4, 5])

  putStr "\nThe reverse list of [1, 2, 3, 4, 5]: "
  print(reverse [1, 2, 3, 4, 5])

  putStr "\nA new list that contains both [1, 2, 3, 4, 5] and [6, 7]: "
  print([1, 2, 3] ++ [4, 5])

  putStr "\nSum of [1, 2, 3, 4, 5]: "
  print(sum [1, 2, 3, 4, 5])

  putStr "\nProduct of [1, 2, 3, 4, 5]: "
  print(product [1, 2, 3, 4, 5])

  putStr "\nDouble of 2: "
  print(double 2)

  putStr "\nQuadruple of 2: "
  print(quadruple 2)

  putStr "\nThe first 4 items of [1, 2, 3, 4, 5]: "
  print(take (double 2) [1, 2, 3, 4, 5])

  putStr "\nFactorial of 10: "
  print(factorial 10)

  putStr "\nAverage of [1, 2, 3, 4, 5]: "
  print(average [1, 2, 3, 4, 5])
