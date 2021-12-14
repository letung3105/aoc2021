import Data.Bits
import Data.Char
import qualified Data.Map as Map
import System.Environment
import System.IO

main :: IO ()
main = do
  args <- getArgs
  input <- readFile . head $ args
  let ans01 = part01 input
      ans02 = part02 input
  print ans01
  print ans02

part01 :: String -> Int
part01 input =
  toDec gamma * toDec sigma
  where
    bitSeqs = lines input
    onesCount = countOnesAll Map.empty bitSeqs
    gamma = bitSeqMostCommon onesCount (length bitSeqs)
    sigma = bitSeqLeastCommon onesCount (length bitSeqs)

part02 :: String -> Int
part02 input = o2rating bitSeqs 0 * co2rating bitSeqs 0
  where
    bitSeqs = lines input
    o2rating [bitSeq] _ = toDec bitSeq
    o2rating bitSeqs pos =
      o2rating
        (retain bitSeqs pos (getLifeSupportCriteria bitSeqs pos bitSeqMostCommon))
        (pos + 1)
    co2rating [bitSeq] _ = toDec bitSeq
    co2rating bitSeqs pos =
      co2rating
        (retain bitSeqs pos (getLifeSupportCriteria bitSeqs pos bitSeqLeastCommon))
        (pos + 1)

getLifeSupportCriteria :: [[Char]] -> Int -> (Map.Map Int Int -> Int -> String) -> Char
getLifeSupportCriteria bitSeqs pos makeBitSeq =
  makeBitSeq
    (countOnesAll Map.empty bitSeqs)
    (length bitSeqs)
    !! pos

retain :: Eq t => [[t]] -> Int -> t -> [[t]]
retain [] _ _ = []
retain (seq : seqs) pos bitCommon
  | seq !! pos == bitCommon = seq : retain seqs pos bitCommon
  | otherwise = retain seqs pos bitCommon

toDec :: [Char] -> Int
toDec = foldl (\acc x -> acc * 2 + digitToInt x) 0

bitSeqMostCommon :: Map.Map Int Int -> Int -> [Char]
bitSeqMostCommon onesCount numSeqs =
  foldl makeBitStr "" . reverse . Map.toAscList $ onesCount
  where
    makeBitStr bitStr (_, count)
      | count * 2 >= numSeqs = '1' : bitStr
      | otherwise = '0' : bitStr

bitSeqLeastCommon :: Map.Map Int Int -> Int -> [Char]
bitSeqLeastCommon onesCount numSeqs =
  foldl makeBitStr "" . reverse . Map.toAscList $ onesCount
  where
    makeBitStr bitStr (_, count)
      | count * 2 >= numSeqs = '0' : bitStr
      | otherwise = '1' : bitStr

countOnesAll :: Map.Map Int Int -> [[Char]] -> Map.Map Int Int
countOnesAll = foldl (\onesCount s -> countOnes onesCount s 0)

countOnes :: Map.Map Int Int -> [Char] -> Int -> Map.Map Int Int
countOnes onesCount [] pos = onesCount
countOnes onesCount ('1' : bs) pos = countOnes (Map.insertWith (+) pos 1 onesCount) bs (pos + 1)
countOnes onesCount ('0' : bs) pos = countOnes (Map.insertWith (+) pos 0 onesCount) bs (pos + 1)
