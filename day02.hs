import System.Environment
import System.IO

main = do
  args <- getArgs
  d <- readFile . head $ args
  let ans01 = part01 d
      ans02 = part02 d
  print ans01
  print ans02

part01 = uncurry (*) . foldl runcmd (0 :: Int, 0 :: Int) . fmap words . lines
  where
    runcmd (x, y) [cmd, offset] = case cmd of
      "forward" -> (x + read offset, y)
      "down" -> (x, y + read offset)
      "up" -> (x, y - read offset)

part02 = coordprod . foldl runcmd (0 :: Int, 0 :: Int, 0 :: Int) . fmap words . lines
  where
    coordprod (x, y, _) = x * y
    runcmd (x, y, z) [cmd, offset] = case cmd of
      "forward" -> (x + read offset, y + read offset * z, z)
      "down" -> (x, y, z + read offset)
      "up" -> (x, y, z - read offset)