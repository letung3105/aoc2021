import Data.Bits
import Data.Char
import qualified Data.Map as Map
import System.Environment
import System.IO

main = do
  args <- getArgs
  input <- readFile . head $ args
  let ans01 = part01 input
      ans02 = part02 input
  print ans01
  print ans02

part01 input =
  toDec gamma * toDec sigma
  where
    bitSeqs = lines input
    onesCount = countOnesAll Map.empty bitSeqs
    gamma = bitSeqMostCommon onesCount (length bitSeqs)
    sigma = bitSeqLeastCommon onesCount (length bitSeqs)

part02 input = o2rating (lines input) 0 * co2rating (lines input) 0
  where
    o2rating [o2] _ = toDec o2
    o2rating o2 pos =
      o2rating
        (retain o2 pos (getLifeSupportCriteria o2 pos bitSeqMostCommon))
        (pos + 1)
    co2rating [co2] _ = toDec co2
    co2rating co2 pos =
      co2rating
        (retain co2 pos (getLifeSupportCriteria co2 pos bitSeqLeastCommon))
        (pos + 1)

getLifeSupportCriteria bitSeqs pos makeBitSeq =
  makeBitSeq
    (countOnesAll Map.empty bitSeqs)
    (length bitSeqs)
    !! pos

retain [] _ _ = []
retain (seq : seqs) pos bitCommon =
  if seq !! pos == bitCommon
    then seq : retain seqs pos bitCommon
    else retain seqs pos bitCommon

toDec = foldl (\acc x -> acc * 2 + digitToInt x) 0

bitSeqMostCommon onesCount numSeqs =
  foldl makeBitStr "" . reverse . Map.toAscList $ onesCount
  where
    makeBitStr bitStr (_, count)
      | count * 2 >= numSeqs = '1' : bitStr
      | otherwise = '0' : bitStr

bitSeqLeastCommon onesCount numSeqs =
  foldl makeBitStr "" . reverse . Map.toAscList $ onesCount
  where
    makeBitStr bitStr (_, count)
      | count * 2 >= numSeqs = '0' : bitStr
      | otherwise = '1' : bitStr

countOnesAll = foldl (\onesCount s -> countOnes onesCount s 0)

countOnes onesCount [] pos = onesCount
countOnes onesCount ('1' : bs) pos = countOnes (Map.insertWith (+) pos 1 onesCount) bs (pos + 1)
countOnes onesCount ('0' : bs) pos = countOnes (Map.insertWith (+) pos 0 onesCount) bs (pos + 1)
