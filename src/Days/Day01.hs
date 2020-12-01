module Days.Day01 (runDay) where

{- ORMOLU_DISABLE -}
import Control.Monad (join)
import Debug.Trace
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
inputParser = many1' (decimal <* "\n")

------------ TYPES ------------
type Input = [ Int ]

type OutputA = Int

type OutputB = Int

------------ PART A ------------
partA :: Input -> OutputA
partA (first:numbers) =
  let
    trynext first numbers@(next:nexts) =
      let
        attempt = trynumber first numbers
      in
        if isJust attempt then
          attempt
        else
          trynext next nexts

    trynumber number others =
      foldr (\next sofar ->
               if isJust sofar then
                 sofar
               else
                 if next + number == 2020 then
                   Just (next, number)
                 else
                   Nothing
            ) Nothing others
  in
    case trynext first numbers of
      Nothing -> 0
      Just (one, two) -> one * two

------------ PART B ------------
partB :: Input -> OutputB
partB (first:numbers) =
  let
    trynext first numbers@(next:nexts) =
      let
        attempt = trynumber first numbers
      in
        if isJust attempt then
          attempt
        else
          trynext next nexts

    trynumber number others@(first:rest) =
      let
        subs :: [ Int ] -> [[ Int ]]
        subs total@(x:xs) =
          let
            numberofrounds = length total - 1

            findSubs (x:xs) round =
              if round > numberofrounds then
                []
              else
                (sublist x xs) ++ (findSubs (xs ++ [x]) (round + 1))
          in
            findSubs total 0

        sublist :: Int -> [ Int ] -> [[ Int ]]
        sublist x xs =
          foldr (\next sofar ->
                   if next + x < 2020 then
                     [x, next] : sofar
                   else
                     sofar
                ) [] xs
      in
        foldr (\(nexta:nextb:[]) sofar ->
                 if isJust sofar then
                   sofar
                 else
                   if nexta + nextb + number == 2020 then
                     Just (nexta, nextb, number)
                   else
                     Nothing
              ) Nothing (subs others)
  in
    case trynext first numbers of
      Nothing -> 0
      Just (one, two, three) -> one * two * three
