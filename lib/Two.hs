{-# LANGUAGE OverloadedStrings #-}

module Two where

import Data.List.Split (chunksOf)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

data Choice = Rock | Paper | Scissors deriving (Eq, Show)

run :: IO Integer
run = do
  contents <- TIO.readFile "inputs/Two.txt"
  let choices = chunksOf 2 . map parseChoice . T.words $ contents
  return . sum . map roundOutcome $ choices

roundOutcome :: [Choice] -> Integer
roundOutcome [opponentChoice, ourChoice] = pointsFromChoice ourChoice + pointsFromOutcome (compare ourChoice opponentChoice)
  where
    pointsFromChoice :: Choice -> Integer
    pointsFromChoice Rock = 1
    pointsFromChoice Paper = 2
    pointsFromChoice Scissors = 3

    pointsFromOutcome :: Ordering -> Integer
    pointsFromOutcome LT = 0
    pointsFromOutcome EQ = 3
    pointsFromOutcome GT = 6

parseChoice :: Text -> Choice
parseChoice choice
  | choice `elem` ["A", "X"] = Rock
  | choice `elem` ["B", "Y"] = Paper
  | otherwise = Scissors

instance Ord Choice where
  Rock `compare` Paper = LT
  Rock `compare` Scissors = GT
  Rock `compare` Rock = EQ
  Paper `compare` Paper = EQ
  Paper `compare` Scissors = LT
  Paper `compare` Rock = GT
  Scissors `compare` Paper = GT
  Scissors `compare` Scissors = EQ
  Scissors `compare` Rock = LT
