import Data.Bifunctor
import Data.Binary.Get (isEmpty)
import Data.Foldable (maximumBy)
import qualified Data.Set as Set
import Distribution.Compat.CharParsing (upper)
import System.Environment
import System.IO

main :: IO ()
main = do
  args <- getArgs
  contents <- readFile . head $ args
  let ans01 = part01 contents
      ans02 = part02 contents
  print ans01
  printDots ans02

-- print out the dots to get the answer for part 2
printDots :: Set.Set (Int, Int) -> IO ()
printDots dots = do
  let xmax = fst $ maximumBy (\(x1, _) (x2, _) -> compare x1 x2) dots
      ymax = snd $ maximumBy (\(_, y1) (_, y2) -> compare y1 y2) dots
      printDots' x y
        | y > ymax = do return ()
        | x > xmax = do
          putStr "\n"
          printDots' 0 (y + 1)
        | otherwise =
          if Set.member (x, y) dots
            then do
              putStr "#"
              printDots' (x + 1) y
            else do
              putStr "."
              printDots' (x + 1) y
  printDots' 0 0

part01 :: String -> Int
part01 s = length $ fold [head folds] dots
  where
    (dots, folds) = parseInput s

part02 :: String -> Set.Set (Int, Int)
part02 s = fold folds dots
  where
    (dots, folds) = parseInput s

-- apply the sequence of folds
fold :: [(Char, Int)] -> Set.Set (Int, Int) -> Set.Set (Int, Int)
fold [] dots = dots
fold ((axis, line) : folds) dots =
  case axis of
    'x' -> fold folds (foldLeft line dots)
    'y' -> fold folds (foldUp line dots)

-- shift the coordinate system such that the given line is our new x-axis, mirror all the
-- dots in the plane below the new axis, then shift back to the original coordinate system
foldUp :: Int -> Set.Set (Int, Int) -> Set.Set (Int, Int)
foldUp line dots = offsetY line $ Set.union upperPlane lowerPlaneFolded
  where
    dotsOffset = offsetY (-line) dots
    upperPlane = Set.filter (\(x, y) -> y <= 0) dotsOffset
    lowerPlane = Set.filter (\(x, y) -> y > 0) dotsOffset
    lowerPlaneFolded = Set.map (Data.Bifunctor.second negate) lowerPlane

-- shift the coordinate system such that the given line is our new y-axis, mirror all the
-- dots in the plane to the left of the new y-axis, then shift back to the original coordinate system
foldLeft :: Int -> Set.Set (Int, Int) -> Set.Set (Int, Int)
foldLeft line dots = offsetX line $ Set.union leftPlane rightPlaneFolded
  where
    dotsOffset = offsetX (-line) dots
    leftPlane = Set.filter (\(x, y) -> x < 0) dotsOffset
    rightPlane = Set.filter (\(x, y) -> x >= 0) dotsOffset
    rightPlaneFolded = Set.map (Data.Bifunctor.first negate) rightPlane

-- linear transformation along the x-axis
offsetX :: Int -> Set.Set (Int, Int) -> Set.Set (Int, Int)
offsetX offset = Set.map (\(x, y) -> (x + offset, y))

-- linear transformation along the y-axis
offsetY :: Int -> Set.Set (Int, Int) -> Set.Set (Int, Int)
offsetY offset = Set.map (\(x, y) -> (x, y + offset))

-- parse problem's input
parseInput :: String -> (Set.Set (Int, Int), [(Char, Int)])
parseInput s =
  let (dots, folds) = break (== "") . lines $ s
   in (Set.fromList . map parseDot $ dots, map parseFold . dropWhile (== "") $ folds)

-- parse the dots' coordinates
parseDot :: String -> (Int, Int)
parseDot s =
  let (x, y) = tupleFromArray2 . wordsWhen (== ',') $ s
   in (read x, read y)

-- parse the sequence of folds
parseFold :: String -> (Char, Int)
parseFold s =
  let (x, y) = tupleFromArray2 . wordsWhen (== '=') . last . words $ s
   in (head x, read y)

-- takes 2 elements array and return a 2-tuple
tupleFromArray2 :: [a] -> (a, a)
tupleFromArray2 [x, y] = (x, y)

-- split string by delimiter
wordsWhen :: (Char -> Bool) -> String -> [String]
wordsWhen p s = case dropWhile p s of
  "" -> []
  s' -> w : wordsWhen p s''
    where
      (w, s'') = break p s'