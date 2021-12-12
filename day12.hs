import Data.Char (isLower, isUpper)
import qualified Data.Map as Map
import qualified Data.Set as Set
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
part01 = countPathsToEnd . readInput

part02 :: String -> Int
part02 = countPathsToEnd' . readInput

-- Count the number of ways that we can go from "start" to "end"
countPathsToEnd :: Graph -> Int
countPathsToEnd graph = count graph (Set.singleton "start") "start"
  where
    visit x visited graph =
      case Map.lookup x graph of
        Nothing -> Set.empty
        Just xs -> Set.difference xs visited

    count graph visited vert
      | vert == "end" = 1 -- count 1 if reach "end"
      | otherwise =
        let -- add the vertex to the listed of visited node if it's not a "small cave"
            visited' = if isSmallCave vert then Set.insert vert visited else visited
            -- all the directly connected vertices that have not been visited
            vertices = visit vert visited' graph
         in -- this will loop indefinitely when there's a cycle in the graph
            if Set.null vertices
              then 0 -- no vertices left to visit
              else foldl (\s v -> s + count graph visited' v) 0 vertices

-- Count the number of ways that we can go from "start" to "end"
countPathsToEnd' :: Graph -> Int
countPathsToEnd' graph = count graph (Map.singleton "start" 1) "start"
  where
    visit x visited graph =
      case Map.lookup x graph of
        Nothing -> Set.empty
        Just xs ->
          let visited' =
                if any (>= 2) . Map.elems $ visited
                  then Map.keysSet visited
                  else Map.keysSet . Map.filterWithKey (\k _ -> not . isSmallCave $ k) $ visited
           in Set.difference xs visited'

    count graph visited vert
      | vert == "end" = 1 -- count 1 if reach "end"
      | otherwise =
        let -- update map of visits counts
            visited' = if isSmallCave vert then Map.insertWith (+) vert 1 visited else visited
            -- all the directly connected vertices that have not been visited
            vertices = visit vert visited' graph
         in -- this will loop indefinitely when there's a cycle in the graph
            if Set.null vertices
              then 0 -- no vertices left to visit
              else foldl (\s v -> s + count graph visited' v) 0 vertices

isSmallCave :: String -> Bool
isSmallCave s = s /= "start" && s /= "end" && all isLower s

readInput :: String -> Graph
readInput = foldl (flip addBidirectionalEdge) Map.empty . map readEdge . lines

readEdge :: String -> (String, String)
readEdge s = let [v1, v2] = wordsWhen (== '-') s in (v1, v2)

wordsWhen :: (Char -> Bool) -> String -> [String]
wordsWhen p s = case dropWhile p s of
  "" -> []
  s' -> w : wordsWhen p s''
    where
      (w, s'') = break p s'

-- Represent the puzzle map as a graph using adjacency list
type Graph = Map.Map String (Set.Set String)

addBidirectionalEdge :: (String, String) -> Graph -> Graph
addBidirectionalEdge (v1, v2) =
  Map.insertWith Set.union v2 (Set.singleton v1)
    . Map.insertWith Set.union v1 (Set.singleton v2)
