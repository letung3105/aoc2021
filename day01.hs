import System.Environment
import System.IO

main = do
  args <- getArgs
  mapM putStrLn args

result01 = processData "../input/day01.txt" day01Part01

result02 = processData "../input/day01.txt" day01Part02

processData fpath procedure = do
  fhandle <- openFile fpath ReadMode
  contents <- hGetContents fhandle
  let results = procedure contents
  hClose fhandle
  return results

countIncrements (count, prev) curr =
  if prev < curr
    then (count + 1, curr)
    else (count, curr)

day01Part01 puzzleData =
  fst $ foldl countIncrements (0, head values) . tail $ values
  where
    values = map read . words $ puzzleData :: [Int]

day01Part02 puzzleData =
  fst $ foldl countIncrements (0, head rollingsums) . tail $ rollingsums
  where
    values = map read . words $ puzzleData :: [Int]
    rollingsums = zipWith3 (\x y z -> x + y + z) values (drop 1 values) (drop 2 values)
