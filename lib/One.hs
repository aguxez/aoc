{-# LANGUAGE OverloadedStrings #-}

module One where

import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import Data.Text.Read (decimal)
import Data.List (maximumBy)

run :: IO Integer
run = do
  contents <- BL.readFile "inputs/One.txt"
  let textCont = fromRight "" . decodeUtf8' . BL.toStrict $ contents
  return . findHighestCalories . parseAndAddCalories . indexGroup . T.splitOn "\n\n" $ textCont

indexGroup :: [Text] -> [(Integer, Text)]
indexGroup = zip [0..]

parseAndAddCalories :: [(Integer, Text)] -> [(Integer, Integer)]
parseAndAddCalories = map (\(index, text) -> (index, parseAndSum $ T.words text))
  where
    parseAndSum :: [Text] -> Integer
    parseAndSum = foldl' parseAndSum' 0
    
    parseAndSum' :: Integer -> Text -> Integer
    parseAndSum' total textNum = case decimal textNum of
      Right (num, _) -> num + total
      Left _ -> total

findHighestCalories :: [(Integer, Integer)] -> Integer
findHighestCalories = snd . maximumBy (\(_, elf1Calories) (_, elf2Calories) -> compare elf1Calories elf2Calories)
