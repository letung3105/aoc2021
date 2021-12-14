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
  play calledValues boards
  where
    (calledValues, boards) = parseInput input
    play [] _ = 0
    play _ [] = 0
    play (calledValue : calledValues) boards =
      let boardsMarked = markBoards calledValue boards
       in case getWinningBoards boardsMarked of
            [] -> play calledValues boardsMarked
            boards -> calledValue * (score . head $ boards)

part02 input =
  play calledValues boards 0
  where
    (calledValues, boards) = parseInput input
    play [] _ finalScore = finalScore
    play _ [] finalScore = finalScore
    play (calledValue : calledValues) boards finalScore =
      let boardsMarked = markBoards calledValue boards
       in case getWinningBoards boardsMarked of
            [] -> play calledValues boardsMarked finalScore
            boards ->
              play
                calledValues
                (filter (`notElem` boards) boardsMarked)
                (calledValue * (score . head $ boards))

parseInput = parse [] [] . lines
  where
    parse datalist datacurr [] =
      let ([calledValues] : boards) = reverse (reverse datacurr : datalist)
       in ( map read . wordsWhen (== ',') $ calledValues,
            map parseBoard boards
          )
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
    getScore score (val, marker)
      | marker = score
      | otherwise = score + val
    rowScore totalScore (row, rowMarkers) =
      foldl getScore totalScore (zip row rowMarkers)

hasWon (BingoBoard _ rowsMarkers) = check rowsMarkers || check (transpose rowsMarkers)
  where
    check [] = False
    check (rowMarkers : rowsMarkers) = and rowMarkers || check rowsMarkers

getWinningBoards = get []
  where
    get winningBoards [] = winningBoards
    get winningBoards (board : boards)
      | hasWon board = get (board : winningBoards) boards
      | otherwise = get winningBoards boards

markBoard value (BingoBoard rows rowsMarkers) =
  BingoBoard rows (mark rows rowsMarkers)
  where
    mark [] [] = []
    mark (row : rows) (rowMarkers : rowsMarkers) =
      markRow row rowMarkers : mark rows rowsMarkers
    markRow [] [] = []
    markRow (v : vs) (m : ms)
      | v == value = True : markRow vs ms
      | otherwise = m : markRow vs ms

markBoards _ [] = []
markBoards value (board : boards) = markBoard value board : markBoards value boards

parseBoard = parse [] []
  where
    parse rows rowsMarkers [] = BingoBoard (reverse rows) (reverse rowsMarkers)
    parse rows rowsMarkers (s : ss) =
      let row = map read . words $ s
          rowMarkers = replicate (length row) False
       in parse (row : rows) (rowMarkers : rowsMarkers) ss
