module Days.Day12 (runDay) where

{- ORMOLU_DISABLE -}
import Debug.Trace (trace)
import Prelude hiding (Left, Right)
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
  many1 $ choice [ char 'N' *> pure North <*> decimal <* skipSpace
                 , char 'S' *> pure South <*> decimal <* skipSpace
                 , char 'E' *> pure East <*> decimal <* skipSpace
                 , char 'W' *> pure West <*> decimal <* skipSpace
                 , char 'L' *> pure Left <*> decimal <* skipSpace
                 , char 'R' *> pure Right <*> decimal <* skipSpace
                 , char 'F' *> pure Forward <*> decimal <* skipSpace
                 ]

------------ TYPES ------------
type Input = [ Actions ]

type OutputA = Int

type OutputB = Int

data Actions
  = North Int
  | South Int
  | East Int
  | West Int
  | Left Int
  | Right Int
  | Forward Int
  deriving (Show)

data Direction
  = N
  | S
  | E
  | W
  deriving (Show)

data Ship = Ship { shipDirection :: Direction
                 , shipX :: Int
                 , shipY :: Int
                 } deriving (Show)

startingShip = Ship { shipDirection = E
                    , shipX = 0
                    , shipY = 0
                    }

rotateShipLeft ship 0 = ship
rotateShipLeft ship 90 =
  case shipDirection ship of
    N -> ship { shipDirection = W}
    S -> ship { shipDirection = E}
    E -> ship { shipDirection = N}
    W -> ship { shipDirection = S}
rotateShipLeft ship x =
  case shipDirection ship of
    N -> rotateShipLeft (ship { shipDirection = W}) (x - 90)
    S -> rotateShipLeft (ship { shipDirection = E}) (x - 90)
    E -> rotateShipLeft (ship { shipDirection = N}) (x - 90)
    W -> rotateShipLeft (ship { shipDirection = S}) (x - 90)

rotateShipRight ship 0 = ship
rotateShipRight ship 90 =
  case shipDirection ship of
    N -> ship { shipDirection = E}
    S -> ship { shipDirection = W}
    E -> ship { shipDirection = S}
    W -> ship { shipDirection = N}
rotateShipRight ship x =
  case shipDirection ship of
    N -> rotateShipRight (ship { shipDirection = E}) (x - 90)
    S -> rotateShipRight (ship { shipDirection = W}) (x - 90)
    E -> rotateShipRight (ship { shipDirection = S}) (x - 90)
    W -> rotateShipRight (ship { shipDirection = N}) (x - 90)

------------ PART A ------------
partA :: Input -> OutputA -- 2280
partA actions =
  let
    endShip =
      foldl' (\sofar next ->
                case next of
                  North units ->
                    sofar { shipY = (shipY sofar - units) }
                  South units ->
                    sofar { shipY = (shipY sofar + units) }
                  East units ->
                    sofar { shipX = (shipX sofar - units) }
                  West units ->
                    sofar { shipX = (shipX sofar + units) }
                  Left units ->
                    rotateShipLeft sofar units
                  Right units ->
                    rotateShipRight sofar units
                  Forward units ->
                    case shipDirection sofar of
                      N ->
                        sofar { shipY = (shipY sofar - units) }
                      S ->
                        sofar { shipY = (shipY sofar + units) }
                      E ->
                        sofar { shipX = (shipX sofar - units) }
                      W ->
                        sofar { shipX = (shipX sofar + units) }
             ) startingShip actions
  in
    abs (shipX endShip) + abs (shipY endShip)

------------ PART B ------------

data WayPoint = WayPoint { wayPointX :: Int
                         , wayPointY :: Int
                         } deriving (Show)

startingWayPoint = WayPoint { wayPointX = (-10)
                            , wayPointY = (-1)
                            }

rotateWayPointLeft wayPoint 0 = wayPoint
rotateWayPointLeft wayPoint 90 =
  let
    x = wayPointX wayPoint
    y = wayPointY wayPoint
  in
    wayPoint { wayPointX = y * (-1)
             , wayPointY = x
             }
rotateWayPointLeft wayPoint x =
  rotateWayPointLeft (rotateWayPointLeft wayPoint 90) (x - 90)

rotateWayPointRight wayPoint 0 = wayPoint
rotateWayPointRight wayPoint 90 =
  let
    x = wayPointX wayPoint
    y = wayPointY wayPoint
  in
    wayPoint { wayPointX = y
             , wayPointY = x * (-1)
             }
rotateWayPointRight wayPoint x =
  rotateWayPointRight (rotateWayPointRight wayPoint 90) (x - 90)


partB :: Input -> OutputB -- 38693
partB actions =
  let
    (endShip, _) =
      foldl' (\(shipSofar, wayPointSofar) next ->
                case next of
                  North units ->
                    (shipSofar, wayPointSofar { wayPointY = (wayPointY wayPointSofar - units) })
                  South units ->
                    (shipSofar, wayPointSofar { wayPointY = (wayPointY wayPointSofar + units) })
                  East units ->
                    (shipSofar, wayPointSofar { wayPointX = (wayPointX wayPointSofar - units) })
                  West units ->
                    (shipSofar, wayPointSofar { wayPointX = (wayPointX wayPointSofar + units) })
                  Left units ->
                    (shipSofar, rotateWayPointLeft wayPointSofar units)
                  Right units ->
                    (shipSofar, rotateWayPointRight wayPointSofar units)
                  Forward units ->
                    let
                      yUnits = wayPointY wayPointSofar * units
                      xUnits = wayPointX wayPointSofar * units
                    in
                      (shipSofar { shipY = (shipY shipSofar + yUnits)
                                 , shipX = (shipX shipSofar + xUnits)
                                 }, wayPointSofar)

             ) (startingShip, startingWayPoint) actions
  in
    abs (shipX endShip) + abs (shipY endShip)
