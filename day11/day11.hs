import Data.Char
import System.Environment
import System.IO

main :: IO ()
main = do
  args <- getArgs
  contents <- readFile . head $ args
  let ans01 = part01 contents
      ans02 = part02 contents
  print ans01
  print ans02

part01 :: String -> Int
part01 input = calc (0, rdGrid input) 100
  where
    calc (x, _) 0 = x
    calc (x, g) n = let (dx, g') = step g in calc (x + dx, g') (n - 1)

part02 :: String -> Int
part02 input = calc (rdGrid input) 1
  where
    calc g n = let (dx, g') = step g in if dx == 100 then n else calc g' (n + 1)

-- increment the energies and check for flashes
step :: Grid -> (Int, Grid)
step = flash . map (map (+ 1))

-- set the energies of the given positions to 0
resetEnergy :: Grid -> [GridPos] -> Grid
resetEnergy = foldl (\g p -> gridSet g p 0)

-- increment the energies of the given positions by 1 if it's not 0
gainEnergy :: Grid -> [GridPos] -> Grid
gainEnergy = foldl (\g p -> gridUpdate g p (\x -> if x == 0 then x else x + 1))

-- perform a sequence of flashes when the conditions are met
flash :: Grid -> (Int, Grid)
flash = flash' 0
  where
    flash' n g =
      case canFlash g of
        -- stop when flash conditions are not met
        [] -> (n, g)
        ps ->
          -- 1. increment energy of cells that are neighbouring the flashes
          -- 2. reset energies of cells that flashed
          -- 3. recurse
          let excited = filter (inGrid g) . concatMap connect8 $ ps
           in flash' (n + length ps) . resetEnergy (gainEnergy g excited) $ ps

-- recursively check each row in the grid for cells that can flash
canFlash :: Grid -> [GridPos]
canFlash [] = []
canFlash g = canFlash' g 0 0
  where
    canFlash' [] i j = []
    canFlash' (r : rs) i j = zip (repeat i) (canFlashRow r) ++ canFlash' rs (i + 1) j

-- recursively check each cell and find those that can flash
canFlashRow :: [Int] -> [Int]
canFlashRow [] = []
canFlashRow r = canFlashRow' r 0
  where
    canFlashRow' [] _ = []
    canFlashRow' (x : xs) i
      | x > 9 = i : canFlashRow' xs (i + 1)
      | otherwise = canFlashRow' xs (i + 1)

-- get all neighbouring coordinates with 8-connectivity
connect8 :: GridPos -> [GridPos]
connect8 (x, y) =
  filter
    (/= (x, y))
    [ (x + dx, y + dy) | dx <- [-1 .. 1], dy <- [-1 .. 1]
    ]

type Grid = [[Int]]

type GridPos = (Int, Int)

-- read the grid
rdGrid :: String -> Grid
rdGrid = map (map digitToInt) . lines

-- check if a position is in the grid
inGrid :: Grid -> GridPos -> Bool
inGrid g (x, y) = x >= 0 && x < length g && y >= 0 && y < length (head g)

-- set the value of a cell in the grid
gridSet :: Grid -> GridPos -> Int -> Grid
gridSet g (x, y) v = setAt x g (setAt y (g !! x) v)

-- modify the value of a cell in the grid
gridUpdate :: Grid -> GridPos -> (Int -> Int) -> Grid
gridUpdate g (x, y) f = setAt x g (updateAt y (g !! x) f)

-- set the value of an elemenet in a list
setAt :: Int -> [a] -> a -> [a]
setAt n xs v = take n xs ++ [v] ++ drop (n + 1) xs

-- modify the value of an elemenet in a list
updateAt :: Int -> [a] -> (a -> a) -> [a]
updateAt n xs f = take n xs ++ [f (xs !! n)] ++ drop (n + 1) xs
