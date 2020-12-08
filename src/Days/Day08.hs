module Days.Day08 (runDay) where

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
import qualified Program.RunDay as R (runDay)
import Data.Attoparsec.Text
import Data.Void
{- ORMOLU_ENABLE -}

runDay :: Bool -> String -> IO ()
runDay = R.runDay inputParser partA partB

------------ PARSER ------------
inputParser :: Parser Input
inputParser =
  many1 $ choice [ string "nop" *> (pure Nop <* skipSpace) <*> signed decimal <* skipSpace
                 , string "acc" *> (pure Acc <* skipSpace) <*> signed decimal <* skipSpace
                 , string "jmp" *> (pure Jmp <* skipSpace) <*> signed decimal <* skipSpace
                 ]

------------ TYPES ------------
data Instructions
  = Nop Int
  | Acc Int
  | Jmp Int
  deriving (Show)

type Input = [ Instructions ]

type OutputA = Int

type OutputB = Either Int Int

------------ PART A ------------
partA :: Input -> OutputA
partA instructions = -- 1317
  let
    accumuldateUntilRepeat currentInstruction accumulator soFar =
      let
        soFar' =
          Set.insert currentInstruction soFar
      in
        if Set.member currentInstruction soFar then
          accumulator
        else
          case instructions !! currentInstruction of
            Nop _ ->
              accumuldateUntilRepeat (currentInstruction + 1) accumulator soFar'
            Acc toInc ->
              accumuldateUntilRepeat (currentInstruction + 1) (accumulator + toInc) soFar'
            Jmp relativeJmp ->
              accumuldateUntilRepeat (currentInstruction + relativeJmp) accumulator soFar'
  in
    accumuldateUntilRepeat 0 0 (Set.empty)

------------ PART B ------------
partB :: Input -> OutputB -- 1033
partB instructions =
  let
    instructions' = zip [0..] instructions
    instructionMap = Map.fromList instructions'
  in
    foldr (\(idx, instruction) soFar ->
             case soFar of
               Right acc ->
                 soFar
               Left _ ->
                 case instruction of
                   Nop i ->
                     loopsForever $ fmap snd $ Map.toList $ Map.alter (\_ -> Just $ Jmp i) idx instructionMap
                   Jmp i ->
                     loopsForever $ fmap snd $ Map.toList $ Map.alter (\_ -> Just $ Nop i) idx instructionMap
                   _ ->
                     Left 0
          ) (Left 0) instructions'

loopsForever instructions =
  let
    accumuldateUntilRepeat currentInstruction accumulator soFar =
      let
        soFar' =
          Set.insert currentInstruction soFar
      in
        if length instructions == currentInstruction then
          Right accumulator
        else if Set.member currentInstruction soFar then
          Left accumulator
        else
          case instructions !! currentInstruction of
            Nop _ ->
              accumuldateUntilRepeat (currentInstruction + 1) accumulator soFar'
            Acc toInc ->
              accumuldateUntilRepeat (currentInstruction + 1) (accumulator + toInc) soFar'
            Jmp relativeJmp ->
              accumuldateUntilRepeat (currentInstruction + relativeJmp) accumulator soFar'
  in
    accumuldateUntilRepeat 0 0 (Set.empty)
