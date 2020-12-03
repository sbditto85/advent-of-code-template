module Days.Day03 (runDay) where

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

import Data.Text (Text)
import qualified Data.Text as T
import qualified Program.RunDay as R (runDay)
import Data.Attoparsec.Text
import Data.Void
{- ORMOLU_ENABLE -}

runDay :: Bool -> String -> IO ()
runDay = R.runDay inputParser partA partB

------------ PARSER ------------
inputParser :: Parser Input
inputParser =
  many1 $ takeWhile1 (not . isEndOfLine) <* skipSpace

------------ TYPES ------------
type Input = [Text]

type OutputA = Int

type OutputB = Int

------------ PART A ------------
partA :: Input -> OutputA -- 211
partA lines =
  doSlopCheck lines 3 1

doSlopCheck :: Input -> Int -> Int -> Int
doSlopCheck lines increaseColumnBy increaseRowBy =
  let
    lineLength = T.length (lines !! 0)

    numRows = length lines

    isTree currentRow currentColumn =
      T.index (lines !! currentRow) currentColumn == '#'

    handleColumnOverflow columnSize =
      if columnSize >= lineLength then
        columnSize - lineLength
      else
        columnSize

    helper currentRow currentColumn =
      if currentRow >= numRows then
        0
      else
        let
          nextRow =
            currentRow + increaseRowBy

          nextColumn =
            handleColumnOverflow $ currentColumn + increaseColumnBy

        in
          if isTree currentRow currentColumn then
            1 + helper nextRow nextColumn
          else
            helper nextRow nextColumn
  in
    helper 0 0


------------ PART B ------------
partB :: Input -> OutputB -- 3584591857
partB lines =
  product [ doSlopCheck lines 1 1
          , doSlopCheck lines 3 1
          , doSlopCheck lines 5 1
          , doSlopCheck lines 7 1
          , doSlopCheck lines 1 2
          ]
