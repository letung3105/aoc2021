import Data.List
import qualified Data.Map as Map
import System.Environment
import System.IO

main = do
  args <- getArgs
  input <- readFile . head $ args
  let ans01 = part01 input
  print ans01

part01 input =
  play (map read . wordsWhen (== ',') $ calledValues) (map parseBoard boards)
  where
    ([calledValues] : boards) = parseInput input
    play [] boards = 0
    play (calledValue : calledValues) boards =
      let boardsMarked = markBoards calledValue boards
       in case getWinningBoard boardsMarked of
            Just board -> calledValue * score board
            Nothing -> play calledValues boardsMarked

parseInput = parse [] [] . lines
  where
    parse datalist datacurr [] = reverse (reverse datacurr : datalist)
    parse datalist datacurr (d : ds) =
      case d of
        "" -> parse (reverse datacurr : datalist) [] ds
        s -> parse datalist (d : datacurr) ds

wordsWhen p s = case dropWhile p s of
  "" -> []
  s' -> w : wordsWhen p s''
    where
      (w, s'') = break p s'

data BingoBoard = BingoBoard [[Int]] [[Bool]] deriving (Show)

score (BingoBoard rows rowsMarkers) = foldl rowScore 0 (zip rows rowsMarkers)
  where
    getScore score (val, marker) =
      if marker
        then score
        else score + val
    rowScore totalScore (row, rowMarkers) =
      foldl getScore totalScore (zip row rowMarkers)

hasWon (BingoBoard _ rowsMarkers) = check rowsMarkers || check (transpose rowsMarkers)
  where
    check [] = False
    check (rowMarkers : rowsMarkers) = and rowMarkers || check rowsMarkers

getWinningBoard [] = Nothing
getWinningBoard (board : boards) =
  if hasWon board
    then Just board
    else getWinningBoard boards

markBoard value (BingoBoard rows rowsMarkers) =
  BingoBoard rows (mark rows rowsMarkers)
  where
    mark [] [] = []
    mark (row : rows) (rowMarkers : rowsMarkers) =
      markRow row rowMarkers : mark rows rowsMarkers
    markRow [] [] = []
    markRow (v : vs) (m : ms) =
      if v == value
        then True : markRow vs ms
        else m : markRow vs ms

markBoards _ [] = []
markBoards value (board : boards) = markBoard value board : markBoards value boards

parseBoard = parse [] []
  where
    parse rows rowsMarkers [] = BingoBoard (reverse rows) (reverse rowsMarkers)
    parse rows rowsMarkers (s : ss) =
      let row = map read . words $ s
          rowMarkers = replicate (length row) False
       in parse (row : rows) (rowMarkers : rowsMarkers) ss
