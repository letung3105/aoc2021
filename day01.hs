import System.Environment
import System.IO

main = do
  args <- getArgs
  contents <- readFile . head $ args
  let ans01 = day01Part01 contents
      ans02 = day01Part02 contents
  print ans01
  print ans02

day01Part01 puzzleData =
  fst $ foldl countIncrements (0::Int, head values) . tail $ values
  where
    values = map read . words $ puzzleData :: [Int]

day01Part02 puzzleData =
  fst $ foldl countIncrements (0::Int, head rollingsums) . tail $ rollingsums
  where
    values = map read . words $ puzzleData :: [Int]
    rollingsums = zipWith3 (\x y z -> x + y + z) values (drop 1 values) (drop 2 values)

countIncrements (count, prev) curr =
  if prev < curr
    then (count + 1, curr)
    else (count, curr)
