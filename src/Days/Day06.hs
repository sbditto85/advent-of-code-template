module Days.Day06 (runDay) where

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

import qualified Data.Set as S
import Control.Monad (join)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Program.RunDay as R (runDay)
import Data.Attoparsec.Text
import qualified Data.Attoparsec.Text as DAT
import Data.Void
{- ORMOLU_ENABLE -}

runDay :: Bool -> String -> IO ()
runDay = R.runDay inputParser partA partB

------------ PARSER ------------
inputParser :: Parser Input
inputParser =
  let
    handleNewGroup = do
      nextChar <- peekChar'
      if nextChar == '\n' then
        fail "done"
      else
        skipSpace

    handleNewLine = do
      nextChar <- peekChar'
      if nextChar == '\n' then do
        DAT.take 1
        pure ()
      else
        skipSpace

  in do
    mls <- many1 $ choice [ fmap Just $ many1 $ handleNewGroup *> (many1 letter) <* handleNewLine
                          , pure Nothing <* handleNewLine
                          ]
    pure $ catMaybes mls

------------ TYPES ------------
type Input = [[[Char]]]

type OutputA = Int

type OutputB = Int

------------ PART A ------------
partA :: Input -> OutputA -- 6878
partA groups =
  let
    sets = fmap (S.fromList . join) groups
  in
    sum $ fmap S.size sets

------------ PART B ------------
partB :: Input -> OutputB -- 3464
partB groups =
  sum $ fmap S.size $ catMaybes $ fmap (\group ->
                                           foldr (\next sofar ->
                                                    case sofar of
                                                      Nothing ->
                                                        Just $ S.fromList next
                                                      Just answers ->
                                                        Just $ answers `S.intersection` (S.fromList next)
                                                 ) Nothing group
                                       ) groups
