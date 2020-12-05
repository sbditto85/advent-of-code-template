module Days.Day05 (runDay) where

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
inputParser =
  many1 $ ( count 10 (choice [ char 'F' *> pure FrontHalf
                             , char 'B' *> pure BackHalf
                             , char 'L' *> pure LeftHalf
                             , char 'R' *> pure RightHalf
                             ])
          ) <* skipSpace

------------ TYPES ------------

data Indicator
  = FrontHalf
  | BackHalf
  | LeftHalf
  | RightHalf
  deriving (Show, Eq)

type Input = [[Indicator]]

type OutputA = Int

type OutputB = Int

------------ PART A ------------
partA :: Input -> OutputA
partA seats =
  maximum $ fmap toUniqueSeatNumber seats


toUniqueSeatNumber :: [Indicator] -> Int -- 835
toUniqueSeatNumber indicators =
  let
    rowIndicators = filter (\i -> i == FrontHalf || i == BackHalf) indicators
    columnIndicators = filter (\i -> i == LeftHalf || i == RightHalf) indicators

    helperRow [] beginning _ = beginning
    helperRow (i:is) beginning end =
      if beginning == end then
        beginning
      else
        case i of
          FrontHalf ->
            let
              newEnd = end - (ceiling $ (fromIntegral (end - beginning)) / 2)
            in
              helperRow is beginning newEnd
          BackHalf ->
            let
              newBeginning = beginning + ceiling ((fromIntegral (end - beginning)) / 2)
            in
              helperRow is newBeginning end
          _ ->
            error "shouldn't have anything other than F or B"

    helperColumn [] beginning _ = beginning
    helperColumn (i:is) beginning end =
      if beginning == end then
        beginning
      else
        case i of
          LeftHalf ->
            let
              newEnd = end - (ceiling $ (fromIntegral (end - beginning)) / 2)
            in
              helperColumn is beginning newEnd
          RightHalf ->
            let
              newBeginning = beginning + ceiling ((fromIntegral (end - beginning)) / 2)
            in
              helperColumn is newBeginning end
          _ ->
            error "shouldn't have anything other than L or R"
  in
    (helperRow rowIndicators 0 127) * 8 + helperColumn columnIndicators 0 7

------------ PART B ------------
partB :: Input -> OutputB -- 649
partB seats =
  let
    sortedSeatNums = sort $ fmap toUniqueSeatNumber seats
    beginning = sortedSeatNums !! 0

    contention = head $ dropWhile (\(i, s) -> i == s) $ zip [beginning..] sortedSeatNums
  in
    if fst contention + 1 == snd contention then
      fst contention
    else
      0
