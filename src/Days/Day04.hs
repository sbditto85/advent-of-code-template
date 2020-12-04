module Days.Day04 (runDay) where

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

import Data.Char (isDigit, isHexDigit)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Program.RunDay as R (runDay)
import Data.Attoparsec.Text
import qualified Data.Attoparsec.Text as DAT
import Data.Void
import Text.Read (readMaybe)
{- ORMOLU_ENABLE -}

runDay :: Bool -> String -> IO ()
runDay = R.runDay inputParser partA partB

------------ PARSER ------------
inputParser :: Parser Input
inputParser =
  let
    handleNewPassport = do
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
    mls <- many1 $ choice [ fmap Just $ many1 $ handleNewPassport *> ((,) <$> (takeWhile1 (/=':') <* DAT.take 1 ) <*> takeWhile1 (\c -> c /= ' ' && c /= '\n')) <* handleNewLine
                          , pure Nothing <* handleNewLine
                          ]
    pure $ catMaybes mls

------------ TYPES ------------
type Input = [[(Text, Text)]]

type OutputA = Int

type OutputB = Int

------------ PART A ------------
partA :: Input -> OutputA -- 239
partA =
  let
    isValid map =
       Map.member "byr" map &&
       Map.member "iyr" map &&
       Map.member "eyr" map &&
       Map.member "hgt" map &&
       Map.member "hcl" map &&
       Map.member "ecl" map &&
       Map.member "pid" map
  in
    sum . fmap (\passport ->
             let
               map = Map.fromList passport
             in
               if isValid map then
                 1
               else
                 0
          )

------------ PART B ------------
partB :: Input -> OutputB -- 188
partB =
  let
    isValid map =
      fromMaybe False (isValidByr <$> Map.lookup "byr" map) &&
      fromMaybe False (isValidIyr <$> Map.lookup "iyr" map) &&
      fromMaybe False (isValidEyr <$> Map.lookup "eyr" map) &&
      fromMaybe False (isValidHgt <$> Map.lookup "hgt" map) &&
      fromMaybe False (isValidHcl <$> Map.lookup "hcl" map) &&
      fromMaybe False (isValidEcl <$> Map.lookup "ecl" map) &&
      fromMaybe False (isValidPid <$> Map.lookup "pid" map)

    isValidByr byrTxt =
      fromMaybe False $ do
        byr <- readMaybe (T.unpack byrTxt) :: Maybe Int
        pure $ byr >= 1920 && byr <= 2002

    isValidIyr iyrTxt =
      fromMaybe False $ do
        iyr <- readMaybe (T.unpack iyrTxt) :: Maybe Int
        pure $ iyr >= 2010 && iyr <= 2020

    isValidEyr eyrTxt =
      fromMaybe False $ do
        eyr <- readMaybe (T.unpack eyrTxt) :: Maybe Int
        pure $ eyr >= 2020 && eyr <= 2030

    hgtParser =
      (,) <$> decimal <*> choice [ string "cm", string "in" ]
    isValidHgt hgtTxt =
      case parse hgtParser hgtTxt of
        Done _ (number, typeTxt) -> if typeTxt == "cm" then
                                      number >= 150 && number <= 193
                                    else if typeTxt == "in" then
                                      number >= 59 && number <= 76
                                    else
                                      False
        _ -> False

    hclParser =
      char '#' *> takeWhile1 isHexDigit <* endOfInput
    isValidHcl hclTxt =
      case parse hclParser hclTxt of
        Done _ digits -> (T.length digits == 6)
        Partial cont -> case cont "" of
                          Done _ digits -> (T.length digits == 6)
                          _ -> False
        _ -> False

    eclParser =
      choice [ string "amb"
             , string "blu"
             , string "brn"
             , string "gry"
             , string "grn"
             , string "hzl"
             , string "oth"
             ]
    isValidEcl eclTxt =
      case parse eclParser eclTxt of
        Done _ _ -> True
        _ -> False

    pidParser =
      takeWhile1 isDigit <* endOfInput
    isValidPid pidTxt =
      case parse pidParser pidTxt of
        Done _ pid -> T.length pid == 9
        Partial cont -> case cont "" of
                          Done _ pid -> T.length pid == 9
                          _ -> False
        _ -> False

  in
    sum . fmap (\passport ->
             let
               map = Map.fromList passport
             in
               if isValid map then
                 1
               else
                 0
          )
