module Days.Day09 (runDay) where

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
partA :: Input -> OutputA -- 2089807806
partA nums =
  let
    preambleLength = 25 -- TODO: CHANGE ME IF EXAMPLE (5) OR REAL (25)

    (preamble, remaining) = splitAt preambleLength nums

    twoSumTo nums num =
      foldr (\next sofar ->
               if sofar then
                 sofar
               else
                 or $ fmap (\(a, b) ->
                               a + b == num
                           ) $ zip (repeat next) nums
            ) False nums

    findFirstInvalid window [] = 0
    findFirstInvalid window (next:remaining) =
      if not (twoSumTo window next) then
        next
      else
        findFirstInvalid (drop 1 window ++ [next]) remaining
  in
    findFirstInvalid preamble remaining

------------ PART B ------------
partB :: Input -> OutputB -- 245848639
partB nums =
  let
    target :: Int
    target = partA nums -- 2089807806 for real, 127 for example
    possibleNums = filter (<target) nums

    findEndNum :: [Int] -> [Int] -> Maybe [Int]
    findEndNum _ [] = Nothing
    findEndNum valsSofar (next:rest) =
      let
        valsSofar' :: [Int]
        valsSofar' =
          next : valsSofar

        sum' =
          sum valsSofar'
      in
        if sum' > target then
          Nothing
        else if sum' == target then
          Just valsSofar'
        else
          findEndNum valsSofar' rest

    findRun [] = [0]
    findRun (beginning:rest) =
      case findEndNum [beginning] rest of
        Nothing ->
          findRun rest
        Just vals ->
          vals

    vals = findRun nums
  in
    (minimum vals) + (maximum vals)
