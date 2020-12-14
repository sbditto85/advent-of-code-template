module Days.Day14 (runDay) where

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

import Data.Text (Text)
import qualified Data.Text as T
import qualified Program.RunDay as R (runDay)
import Data.Attoparsec.Text hiding (take)
import Data.Void
import Data.Bits
{- ORMOLU_ENABLE -}

runDay :: Bool -> String -> IO ()
runDay = R.runDay inputParser partA partB

------------ PARSER ------------
inputParser :: Parser Input
inputParser =
  many1 $ (choice [ (pure Mask <* string "mask = ") <*> (fmap T.pack $ count 36 $ choice [ char 'X'
                                                                                         , char '1'
                                                                                         , char '0'
                                                                                         ])
                  , pure MemSet <*> (string "mem[" *> decimal <* string "] = ") <*> decimal
                  ] <* skipSpace)

------------ TYPES ------------
type Input = [ Instruction ]

type OutputA = Int

type OutputB = Int

data Instruction
  = Mask Text
  | MemSet Int Int
  deriving (Show)

------------ PART A ------------
partA :: Input -> OutputA -- 17934269678453
partA instructions =
  let
    memory :: Map Int Int
    memory = Map.empty

    startingMask = T.pack $ take 36 $ repeat 'X'

    calculateValue :: Text -> Int -> Int
    calculateValue mask value =
      let
        (_, value'') =
          T.foldr (\next (count, value') ->
                     case next of
                       '1' ->
                         (count + 1, value' `setBit` count)
                       '0' ->
                         (count + 1, value' `clearBit` count)
                       _ ->
                         (count + 1, value')
                  ) (0, value) mask

      in
        value''

    processInstruction :: [ Instruction ] -> Map Int Int -> Text -> Map Int Int
    processInstruction [] memory _ = memory
    processInstruction (i:is) memory mask =
      case i of
        Mask newMask ->
          processInstruction is memory newMask
        MemSet location value ->
          let
            newVal =
              calculateValue mask value

            newMemory =
              Map.alter (\_mCurrent ->
                           Just newVal
                        ) location memory
          in
            processInstruction is newMemory mask
  in
    sum . fmap snd . Map.toList $ processInstruction instructions memory startingMask

------------ PART B ------------
partB :: Input -> OutputB -- TODO: this
partB instructions =
  let
    memory :: Map Int Int
    memory = Map.empty

    startingMask = T.pack $ take 36 $ repeat 'X'

    calculateValue :: Text -> Int -> Int
    calculateValue mask value =
      let
        (_, value'') =
          T.foldr (\next (count, value') ->
                     case next of
                       '1' ->
                         (count + 1, value' `setBit` count)
                       '0' ->
                         (count + 1, value' `clearBit` count)
                       _ ->
                         (count + 1, value')
                  ) (0, value) mask

      in
        value''

    processInstruction :: [ Instruction ] -> Map Int Int -> Text -> Map Int Int
    processInstruction [] memory _ = memory
    processInstruction (i:is) memory mask =
      case i of
        Mask newMask ->
          processInstruction is memory newMask
        MemSet location value ->
          let
            newVal =
              calculateValue mask value

            newMemory =
              Map.alter (\_mCurrent ->
                           Just newVal
                        ) location memory
          in
            processInstruction is newMemory mask
  in
    sum . fmap snd . Map.toList $ processInstruction instructions memory startingMask
