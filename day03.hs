import Data.Bits
import qualified Data.Map as Map
import System.Environment
import System.IO

fpathExample = "input/day03_ex.txt"

main = do
  args <- getArgs
  d <- readFile . head $ args
  let ans01 = part01 d
  print ans01

part01 :: String -> Int
part01 d =
  uncurry (*) $ calc 0 0 (Map.toAscList onesCount)
  where
    bitSeqs = lines d
    onesCount = countOnesAll Map.empty bitSeqs
    calc gamma sigma [] = (gamma, sigma)
    calc gamma sigma ((pos, count) : rest) =
      if count * 2 >= length bitSeqs
        then calc (gamma .|. shiftL 1 (length onesCount - pos - 1)) sigma rest
        else calc gamma (sigma .|. shiftL 1 (length onesCount - pos - 1)) rest

countOnes :: Map.Map Int Int -> [Char] -> Int -> Map.Map Int Int
countOnes onesCount [] pos = onesCount
countOnes onesCount ('1' : bs) pos = countOnes (Map.insertWith (+) pos 1 onesCount) bs (pos + 1)
countOnes onesCount ('0' : bs) pos = countOnes onesCount bs (pos + 1)

countOnesAll :: Map.Map Int Int -> [[Char]] -> Map.Map Int Int
countOnesAll = foldl (\onesCount s -> countOnes onesCount s 0)