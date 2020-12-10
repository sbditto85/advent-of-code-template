module Days.Day10 (runDay) where

{- ORMOLU_DISABLE -}
import Debug.Trace (trace)
import Data.List
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Vector (Vector)
import qualified Data.Vector as Vec
import qualified Util.Util as U

import qualified Program.RunDay as R (runDay)
import Data.Attoparsec.Text
import Data.Void
{- ORMOLU_ENABLE -}

runDay :: Bool -> String -> IO ()
runDay = R.runDay inputParser partA partB

------------ PARSER ------------
inputParser :: Parser Input
inputParser = many1 $ decimal <* skipSpace

------------ TYPES ------------
type Input = [Int]

type OutputA = Int

type OutputB = Int

------------ PART A ------------
partA :: Input -> OutputA
partA adapters =
  let
    myAdapter =
      maximum adapters + 3

    sortedAdapters =
      sort (0 : myAdapter : adapters)

    mapOfDifferences =
      Map.fromList . fmap (\xs -> (head xs, length xs)) . group . sort . fmap (\(a, b) -> b - a) $ zip sortedAdapters (drop 1 sortedAdapters)
  in
    trace ("#1 => " ++ show (mapOfDifferences Map.! 1) ++ " #3 => " ++ show (mapOfDifferences Map.! 3)) $ (mapOfDifferences Map.! 1) * (mapOfDifferences Map.! 3)

------------ PART B ------------
-- stolen from https://github.com/AxlLind/AdventOfCode2020/blob/master/src/bin/10.rs
partB :: Input -> OutputB
partB adapters =
  let
    myAdapter =
      maximum adapters + 3

    sortedAdapters =
      sort (myAdapter : adapters)

    cache =
      foldl' (\sofar next ->
             let
               nextVal =
                 fromMaybe 0 (Map.lookup (next - 1) sofar) +
                 fromMaybe 0 (Map.lookup (next - 2) sofar) +
                 fromMaybe 0 (Map.lookup (next - 3) sofar)
             in
               Map.insert next nextVal sofar
          ) (Map.singleton 0 1) sortedAdapters
  in
    cache Map.! myAdapter



-- Part b brute force (TAKES FOREVER)
partB' :: Input -> OutputB
partB' adapters =
  let
    myAdapter =
      maximum adapters + 3

    sortedAdapters =
      sort (0 : myAdapter : adapters)

    permutations :: [Int] -> Int
    permutations [] = 1
    permutations js'@(j:js) =
      let
        handleNextMoves [] = 1
        handleNextMoves [j'] = if j' - j <= 3 && j' == myAdapter then 1 else 0
        handleNextMoves rest@(j':js') =
          if j' - j <= 3 then
            permutations rest  + handleNextMoves js'
          else
            0
      in
        handleNextMoves js
  in
    permutations sortedAdapters
