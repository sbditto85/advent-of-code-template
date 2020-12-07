module Days.Day07 (runDay) where

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
import Data.Attoparsec.Text
import qualified Data.Attoparsec.Text as DAT
import Data.Void
{- ORMOLU_ENABLE -}

runDay :: Bool -> String -> IO ()
runDay = R.runDay inputParser partA partB

------------ PARSER ------------
inputParser :: Parser Input
inputParser = do
  bags <- many1 $ do
    bagType <- manyTill anyChar (string " bags contain ")
    contains <- choice [ many1 $ (,)
                         <$> (decimal <* skipSpace)
                         <*> (fmap T.pack $ manyTill anyChar (choice [ string " bag,"
                                                                     , string " bags,"
                                                                     , string " bag."
                                                                     , string " bags."
                                                                     ]
                                                             ) <* skipSpace
                             )
                       , string "no other bags." *> pure [] <* skipSpace
                       ]
    pure $ (T.pack bagType, contains)
  pure $ Map.fromList bags

------------ TYPES ------------
type Input = Map Text [ (Int, Text) ]

type OutputA = Int

type OutputB = Int

------------ PART A ------------
partA :: Input -> OutputA -- 370
partA bagRules =
  let
    myBag = "shiny gold"

    innerToOuter =
      Map.foldrWithKey (\nextOuter nextInners (sofar :: Map Text [ Text ]) ->
                          foldr (\(_, nextInner) (sofar' :: Map Text [ Text ]) ->
                                   Map.alter (\mOuters ->
                                                case mOuters of
                                                  Nothing ->
                                                    Just [nextOuter]
                                                  Just outers ->
                                                    Just $ nextOuter : outers
                                             ) nextInner sofar'
                                ) sofar nextInners
                       ) Map.empty bagRules

    helper checkedSofar leftToCheck =
      if Set.size leftToCheck > 0 then
        let
          (toCheck, leftToCheck') = Set.deleteFindMin leftToCheck

          checkedSofar' = Set.insert toCheck checkedSofar

          leftToCheck'' =
            case Map.lookup toCheck innerToOuter of
              Nothing ->
                leftToCheck'
              Just moreToCheck ->
                Set.union (Set.fromList moreToCheck) leftToCheck'
        in
          helper checkedSofar' leftToCheck''
      else
        checkedSofar

  in
    Set.size $ helper (Set.empty) (Set.fromList $ fromMaybe [] (Map.lookup myBag innerToOuter))

------------ PART B ------------
partB :: Input -> OutputB -- 29547
partB bagRules =
  let
    myBag = "shiny gold"

    helper countFor =
      case Map.lookup countFor bagRules of
        Nothing ->
          0
        Just innerBags ->
          sum $ fmap (\(numberOf, bagType) ->
                         numberOf + numberOf * helper bagType
                     ) innerBags
  in
    helper myBag
