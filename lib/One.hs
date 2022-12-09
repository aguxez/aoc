{-# LANGUAGE OverloadedStrings #-}

module One where

import Data.List (sortOn)
import Data.Ord (Down)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Text.Read (decimal)

run :: IO Integer
run = do
  contents <- TIO.readFile "inputs/One.txt"
  return . sumTop3Calories . parseAndAddCalories . indexGroup . T.splitOn "\n\n" $ contents

indexGroup :: [Text] -> [(Integer, Text)]
indexGroup = zip [0 ..]

parseAndAddCalories :: [(Integer, Text)] -> [(Integer, Integer)]
parseAndAddCalories = map (\(index, text) -> (index, parseAndSum $ T.words text))
  where
    parseAndSum :: [Text] -> Integer
    parseAndSum = foldl' parseAndSum' 0

    parseAndSum' :: Integer -> Text -> Integer
    parseAndSum' total textNum = case decimal textNum of
      Right (num, _) -> num + total
      Left _ -> total

sumTop3Calories :: [(Integer, Integer)] -> Integer
sumTop3Calories = sum . map snd . take 3 . sortOn (Down . snd)
