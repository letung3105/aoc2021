import System.Environment
import System.IO

main :: IO ()
main = do
  args <- getArgs
  contents <- readFile . head $ args
  let ans01 = part01 contents
      ans02 = part02 contents
  print ans01
  print ans02

part01 :: String -> Int
part01 = countIncrements . map readInt . words

part02 :: String -> Int
part02 = countIncrements . rollingSum3 . map readInt . words

readInt :: String -> Int
readInt = read

rollingSum3 :: [Int] -> [Int]
rollingSum3 xs = zipWith3 (\x y z -> x + y + z) xs (drop 1 xs) (drop 2 xs)

countIncrements :: [Int] -> Int
countIncrements xs = fst . foldl inner (0, head xs) $ tail xs
  where
    inner (count, prev) curr
      | prev < curr = (count + 1, curr)
      | otherwise = (count, curr)
