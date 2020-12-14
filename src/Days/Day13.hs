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

import Debug.Trace (trace)
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
partA :: Input -> OutputA -- 3246
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
-- Needed some help and saw this https://en.wikipedia.org/wiki/Chinese_remainder_theorem#Search_by_sieving but did the code myself, pretty proud even though it took me WAY longer then i'll admit :D
partB :: Input -> OutputB -- 1010182346291467
partB busInfo =
  let
    restraints =
      sortBy (\(a, _) (b, _) -> compare b a) $
      fmap (\(minutesAfter, mBusNumber) ->
              let
                busNumber :: Int
                busNumber =
                  fromMaybe 0 mBusNumber

                mod' :: Int
                mod' =
                  if minutesAfter == 0 then
                    0
                  else
                    busNumber - (minutesAfter `mod` busNumber)
              in
                (mod', busNumber)
           ) $ filter (\(_, mVal) -> isJust mVal) $ zip [0..] (buses busInfo)

    startNumber :: Int
    startNumber =
      fst . head $ trace ("restraints " ++ show restraints) restraints

    baseIteration :: Int
    baseIteration =
      snd . head $ trace ("restraints " ++ show restraints) restraints

    offsetMatches timestamp (offset, busNumber) =
      timestamp `mod` busNumber == offset

    checkNext timestamp toCheck =
      offsetMatches timestamp toCheck

    helper :: Int -> Int -> [(Int, Int)] -> Int
    helper _ t [] = t
    helper i t toCheck'@(next:toCheck) =
      if checkNext t next then
        if toCheck == [] then
          t
        else
          trace ( "Going from " ++ show next ++
                  " which incremented by " ++ show i ++
                  " to increment by " ++ show (i * snd next) ++
                  " at t " ++ show t
                ) $ helper (i * snd next) (t + (i * snd next)) toCheck
      else
        trace ( "Continuing " ++ show next ++
                " incrementing by " ++ show i  ++
                " at t " ++ show t
              ) $ helper i (t + i) toCheck'
  in
    helper baseIteration startNumber (drop 1 restraints)
