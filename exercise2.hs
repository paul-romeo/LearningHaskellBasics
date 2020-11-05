-- | Define functions
-- import Prelude hiding ((||)) -- hide the standard || operator
import Prelude hiding ((||))

halve1 xs = splitAt (length xs `div` 2) xs

halve2 xs = (take n xs, drop n xs)
  where n = length xs `div` 2

halve3 xs = splitAt(div (length xs) 2) xs

safetail1 xs = if null xs then [] else tail xs

safetail2 [] = []
safetail2 (_: xs) = xs

safetail3 xs
  | null xs = []
  | otherwise = tail xs

safetail4 [] = []
safetail4 xs = tail xs

safetail5 [x] = [x]
safetail5 (_ : xs) = xs

(||) :: Bool -> Bool -> Bool
False || b = b
True || _ = True

-- | main program starts
main :: IO()
main = do

-- Display outputs : Note: "show" converts the returned values to string
putStrLn("**********************************")
putStrLn("\n halve1 xs = " ++ show(halve1 [1, 2, 3, 4, 5, 6]))
putStrLn("\n halve2 xs = " ++ show(halve2 [1, 2, 3, 4, 5, 6, 7]))
putStrLn("\n halve3 xs = " ++ show(halve3 [1, 2, 3, 4, 5, 6, 7]))
putStrLn("\n safetail2 [1, 2, 3, 4, 5, 6, 7] = " ++ show(safetail2 [1, 2, 3, 4, 5, 6, 7]))
-- putStrLn("\n safetail2 [] = " ++ show(safetail2 []))
-- print(safetail4 [1])

putStrLn("\n True || False = " ++ show(True || True))
