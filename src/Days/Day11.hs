module Days.Day11 (runDay) where

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
inputParser = do
  rows <- many1 $ do
    cols <- many1 $ choice [ char 'L' *> pure Empty
                           , char '.' *> pure Floor
                           , char '#' *> pure Occupied
                           ]
    skipSpace
    pure $ Vec.fromList cols
  pure $ Vec.fromList rows


------------ TYPES ------------
type Grid = Vector (Vector Seat)

type Input = Grid

type OutputA = Int

type OutputB = Int

data Seat
  = Floor
  | Occupied
  | Empty
  deriving (Show, Eq)

gridVal :: Grid -> Int -> Int -> Seat
gridVal grid x y =
  (grid Vec.! y) Vec.! x

gridValSafe :: Grid -> Int -> Int -> Maybe Seat
gridValSafe grid x y = do
  row <- grid Vec.!? y
  row Vec.!? x

setGridVal :: Grid -> Int -> Int -> Seat -> Grid
setGridVal grid x y newVal =
  let
    row = grid Vec.! y
  in
    grid Vec.// [(y, row Vec.// [(x, newVal)])]

------------ PART A ------------
partA :: Input -> OutputA -- 2261
partA grid =
  let
    yLength = Vec.length grid - 1
    xLength = Vec.length (grid Vec.! 0) - 1

    step grid' =
      let
        checkSlot x y =
          case gridVal grid' x y of
            Empty ->
              let
                hasNeighbor =
                  any (==Occupied) $ catMaybes [ gridValSafe grid' (x - 1) (y - 1) -- up left
                                               , gridValSafe grid' (x)     (y - 1) -- up
                                               , gridValSafe grid' (x + 1) (y - 1) -- up right
                                               , gridValSafe grid' (x - 1) (y)     -- left
                                               , gridValSafe grid' (x + 1) (y)     -- right
                                               , gridValSafe grid' (x - 1) (y + 1) -- down left
                                               , gridValSafe grid' (x)     (y + 1) -- down
                                               , gridValSafe grid' (x + 1) (y + 1) -- down right
                                               ]
              in
                if hasNeighbor then
                  Nothing
                else
                  Just Occupied
            Occupied ->
              let
                occupiedNeighbors =
                  filter (==Occupied) $ catMaybes [ gridValSafe grid' (x - 1) (y - 1) -- up left
                                                  , gridValSafe grid' (x)     (y - 1) -- up
                                                  , gridValSafe grid' (x + 1) (y - 1) -- up right
                                                  , gridValSafe grid' (x - 1) (y)     -- left
                                                  , gridValSafe grid' (x + 1) (y)     -- right
                                                  , gridValSafe grid' (x - 1) (y + 1) -- down left
                                                  , gridValSafe grid' (x)     (y + 1) -- down
                                                  , gridValSafe grid' (x + 1) (y + 1) -- down right
                                                  ]
              in
                if length occupiedNeighbors >= 4 then
                  Just Empty
                else
                  Nothing
            Floor ->
              Nothing

      in
        foldl' (\sofar nextX ->
                  foldl' (\sofar' nextY ->
                            case checkSlot nextX nextY of
                              Nothing ->
                                sofar'
                              Just newVal ->
                                -- sofar'
                                setGridVal sofar' nextX nextY newVal
                         ) sofar [0..yLength]
               ) grid' [0..xLength]

    untilNoChange grid' =
      let
        grid'' = step grid'
      in
        if grid'' == grid' then
          grid'
        else
          untilNoChange grid''

    countOccupied grid' =
      foldl' (\sofar nextX ->
                  foldl' (\sofar' nextY ->
                            case gridVal grid' nextX nextY of
                              Occupied ->
                                sofar' + 1
                              _ ->
                                sofar'
                         ) sofar [0..yLength]
               ) 0 [0..xLength]
  in
    countOccupied $ untilNoChange grid

------------ PART B ------------
partB :: Input -> OutputB -- 2039
partB grid =
  let
    yLength = Vec.length grid - 1
    xLength = Vec.length (grid Vec.! 0) - 1

    findSeat :: Grid -> Int -> (Int -> Int) -> Int -> (Int -> Int) -> Maybe Seat
    findSeat grid' x modx y mody =
      case gridValSafe grid' x y of
        Nothing ->
          Nothing
        Just Floor ->
          findSeat grid' (modx x) modx (mody y) mody
        found ->
          found

    step grid' =
      let
        checkSlot x y =
          case gridVal grid' x y of
            Empty ->
              let
                hasNeighbor =
                  any (==Occupied) $
                    catMaybes [ findSeat grid' (x - 1) (\x -> x-1) (y - 1) (\x -> x-1)  -- up left
                              , findSeat grid' (x)     id          (y - 1) (\x -> x-1)  -- up
                              , findSeat grid' (x + 1) (+1)        (y - 1) (\x -> x-1)  -- up right
                              , findSeat grid' (x - 1) (\x -> x-1) (y)     id           -- left
                              , findSeat grid' (x + 1) (+1)        (y)     id           -- right
                              , findSeat grid' (x - 1) (\x -> x-1) (y + 1) (+1)         -- down left
                              , findSeat grid' (x)     id          (y + 1) (+1)         -- down
                              , findSeat grid' (x + 1) (+1)        (y + 1) (+1)         -- down right
                              ]
              in
                if hasNeighbor then
                  Nothing
                else
                  Just Occupied
            Occupied ->
              let
                occupiedNeighbors =
                  filter (==Occupied) $
                    catMaybes [ findSeat grid' (x - 1) (\x -> x-1) (y - 1) (\x -> x-1)  -- up left
                              , findSeat grid' (x)     id          (y - 1) (\x -> x-1)  -- up
                              , findSeat grid' (x + 1) (+1)        (y - 1) (\x -> x-1)  -- up right
                              , findSeat grid' (x - 1) (\x -> x-1) (y)     id           -- left
                              , findSeat grid' (x + 1) (+1)        (y)     id           -- right
                              , findSeat grid' (x - 1) (\x -> x-1) (y + 1) (+1)         -- down left
                              , findSeat grid' (x)     id          (y + 1) (+1)         -- down
                              , findSeat grid' (x + 1) (+1)        (y + 1) (+1)         -- down right
                              ]
              in
                if length occupiedNeighbors >= 5 then
                  Just Empty
                else
                  Nothing
            Floor ->
              Nothing

      in
        foldl' (\sofar nextX ->
                  foldl' (\sofar' nextY ->
                            case checkSlot nextX nextY of
                              Nothing ->
                                sofar'
                              Just newVal ->
                                -- sofar'
                                setGridVal sofar' nextX nextY newVal
                         ) sofar [0..yLength]
               ) grid' [0..xLength]

    untilNoChange grid' =
      let
        grid'' = step grid'
      in
        if grid'' == grid' then
          grid'
        else
          untilNoChange grid''

    countOccupied grid' =
      foldl' (\sofar nextX ->
                  foldl' (\sofar' nextY ->
                            case gridVal grid' nextX nextY of
                              Occupied ->
                                sofar' + 1
                              _ ->
                                sofar'
                         ) sofar [0..yLength]
               ) 0 [0..xLength]
  in
    countOccupied $ untilNoChange grid
