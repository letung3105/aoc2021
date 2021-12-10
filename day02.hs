import System.Environment
import System.IO

main :: IO ()
main = do
  args <- getArgs
  d <- readFile . head $ args
  let ans01 = part01 d
      ans02 = part02 d
  print ans01
  print ans02

part01 :: String -> Int
part01 = navigate . fmap words . lines

part02 :: String -> Int
part02 = navigate' . fmap words . lines

navigate :: [[String]] -> Int
navigate = uncurry (*) . foldl inner (0, 0)
  where
    inner (x, y) [cmd, offset] = case cmd of
      "forward" -> (x + read offset, y)
      "down" -> (x, y + read offset)
      "up" -> (x, y - read offset)

navigate' :: [[String]] -> Int
navigate' = (\(x, y, _) -> x * y) . foldl inner (0, 0, 0)
  where
    inner (x, y, z) [cmd, offset] = case cmd of
      "forward" -> (x + read offset, y + read offset * z, z)
      "down" -> (x, y, z + read offset)
      "up" -> (x, y, z - read offset)
