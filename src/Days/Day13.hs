module Days.Day13 (runDay) where

{- ORMOLU_DISABLE -}
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
  BusInfo <$> (decimal <* skipSpace) <*> many1 (choice [ Just <$> (decimal <* option ',' (char ','))
                                                       , char 'x' *> pure Nothing <* option ',' (char ',')
                                                       ])

------------ TYPES ------------

data BusInfo = BusInfo { earliest :: Int
                       , buses :: [Maybe Int]
                       } deriving (Show)

type Input = BusInfo

type OutputA = Int

type OutputB = Int

------------ PART A ------------
partA :: Input -> OutputA
partA busInfo =
  let
    target = earliest busInfo

    possibilities :: [(Int, Int)]
    possibilities =
      foldr (\next sofar ->
               (next, (((target `div` next) + 1) * next) - target) : sofar
            ) [] (catMaybes $ buses busInfo)

    selection :: (Int, Int)
    selection =
      minimumBy (\(_, a) (_, b) -> compare a b) possibilities
  in
    (fst selection) * (snd selection)

------------ PART B ------------
partB :: Input -> OutputB
partB busInfo =
  let
    restraints =
      fmap (\(minutesAfter, mBusNumber) ->
              (minutesAfter, fromMaybe 0 mBusNumber)
           ) $ filter (\(_, mVal) -> isJust mVal) $ zip [0..] (buses busInfo)

    baseIteration =
      snd . head $ restraints

    iteration =
      fmap (\mulBy -> mulBy * baseIteration) [1..]

    toCheck =
      drop 1 restraints

    offsetMatches timestamp (offset, busNumber) =
      (timestamp + offset) `mod` busNumber == 0

    check timestamp =
      and $ fmap (offsetMatches timestamp) toCheck

    helper (t:ts) =
      if check t then
        t
      else
        helper ts
  in
    helper iteration
