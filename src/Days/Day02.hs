module Days.Day02 (runDay) where

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
import Data.Text (Text)
import qualified Data.Text as T
{- ORMOLU_ENABLE -}

runDay :: Bool -> String -> IO ()
runDay = R.runDay inputParser partA partB

------------ PARSER ------------
inputParser :: Parser Input
inputParser =
  many1 $ (,,,) <$> decimal <*> (char '-' *> decimal <* anyChar) <*> (anyChar <* string ": ") <*> (takeTill isEndOfLine <* skipSpace)

------------ TYPES ------------
type Input = [(Int, Int, Char, Text)]

type OutputA = Int

type OutputB = Int

------------ PART A ------------
partA :: Input -> OutputA
partA inputs =
  length $ filter (\(min', max', char', password') ->
                     let
                       numChar = T.length $ T.filter (==char') password'
                     in
                       numChar >= min' && numChar <= max'
                  ) inputs

------------ PART B ------------
partB :: Input -> OutputB
partB inputs =
  length $ filter (\(slotOne, slotTwo, char', password') ->
                     let
                       slotOneHas = (T.index password' (slotOne - 1)) == char'
                       slotTwoHas = (T.index password' (slotTwo - 1)) == char'
                     in
                       (slotOneHas && (not slotTwoHas)) || ((not slotOneHas) && slotTwoHas)
                  ) inputs
