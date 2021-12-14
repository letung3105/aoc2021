import qualified Data.Map as Map
import System.Environment
import System.IO

main = do
  args <- getArgs
  input <- readFile $ head args
  let ans01 = part01 input
      ans02 = part02 input
  print ans01
  print ans02

type Point = (Int, Int)

type IntersectionsCount = Map.Map Point Int

part01 :: String -> Int
part01 input =
  Map.size
    . Map.filter (>= 2)
    $ Map.unionWith (+) horzcounts vertcounts
  where
    segments = map parseSegment $ lines input
    horzcounts = countsHorz segments
    vertcounts = countsVert segments

part02 :: String -> Int
part02 input =
  Map.size
    . Map.filter (>= 2)
    . Map.unionWith (+) diagcounts
    $ Map.unionWith (+) horzcounts vertcounts
  where
    segments = map parseSegment $ lines input
    horzcounts = countsHorz segments
    vertcounts = countsVert segments
    diagcounts = countsDiag segments

countsHorz :: [(Point, Point)] -> IntersectionsCount
countsHorz =
  foldl (\counts p -> Map.insertWith (+) p 1 counts) Map.empty
    . concatMap (uncurry drawHorz)
    . horzSegments

countsVert :: [(Point, Point)] -> IntersectionsCount
countsVert =
  foldl (\counts p -> Map.insertWith (+) p 1 counts) Map.empty
    . concatMap (uncurry drawVert)
    . vertSegments

countsDiag :: [(Point, Point)] -> IntersectionsCount
countsDiag =
  foldl (\counts p -> Map.insertWith (+) p 1 counts) Map.empty
    . concatMap (uncurry drawDiag)
    . diagSegments

vertSegments :: [(Point, Point)] -> [(Point, Point)]
vertSegments = filter (\(p1, p2) -> fst p1 == fst p2)

horzSegments :: [(Point, Point)] -> [(Point, Point)]
horzSegments = filter (\(p1, p2) -> snd p1 == snd p2)

diagSegments :: [(Point, Point)] -> [(Point, Point)]
diagSegments = filter (\((x1, y1), (x2, y2)) -> abs (x1 - x2) == abs (y1 - y2))

drawHorz :: Point -> Point -> [Point]
drawHorz (x1, y1) (x2, y2)
  | y1 /= y2 = error "Invalid horizontal segment"
  | otherwise = zip [min x1 x2 .. max x1 x2] (repeat y1)

drawVert :: Point -> Point -> [Point]
drawVert (x1, y1) (x2, y2)
  | x1 /= x2 = error "Invalid vertical segment"
  | otherwise = zip (repeat x1) [min y1 y2 .. max y1 y2]

drawDiag :: Point -> Point -> [Point]
drawDiag (x1, y1) (x2, y2)
  | abs (x1 - x2) /= abs (y1 - y2) = error "Invalid diagonal segment"
  | otherwise = zip xs ys
  where
    xs = if x1 < x2 then [x1 .. x2] else reverse [x2 .. x1]
    ys = if y1 < y2 then [y1 .. y2] else reverse [y2 .. y1]

parseSegment :: String -> (Point, Point)
parseSegment s = (parsePoint p1, parsePoint p2)
  where
    [p1, _, p2] = words s

parsePoint :: String -> Point
parsePoint s = (read x, read y)
  where
    [x, y] = wordsWhen (== ',') s

wordsWhen p s = case dropWhile p s of
  "" -> []
  s' -> w : wordsWhen p s''
    where
      (w, s'') = break p s'
