import Data.List
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
  play (map read . wordsWhen (== ',') $ calledValues) (map parseBoard boards)
  where
    ([calledValues] : boards) = parseInput input
    play [] boards = 0
    play (calledValue : calledValues) boards =
      let boardsMarked = markBoards calledValue boards
       in case getWinningBoard boardsMarked of
            Just board -> calledValue * score board
            Nothing -> play calledValues boardsMarked

part02 input =
  play (map read . wordsWhen (== ',') $ calledValues) (map parseBoard boards) 0
  where
    ([calledValues] : boards) = parseInput input

    removeWinningBoards [] = []
    removeWinningBoards boards =
      case getWinningBoard boards of
        Just board -> removeWinningBoards (filter (/= board) boards)
        Nothing -> boards

    play [] _ finalScore = finalScore
    play _ [] finalScore = finalScore
    play (calledValue : calledValues) boards finalScore =
      let boardsMarked = markBoards calledValue boards
       in case getWinningBoard boardsMarked of
            Just board -> play calledValues (removeWinningBoards boardsMarked) (calledValue * score board)
            Nothing -> play calledValues boardsMarked finalScore

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

data BingoBoard = BingoBoard [[Int]] [[Bool]] deriving (Show, Eq)

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
