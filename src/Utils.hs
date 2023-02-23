module Utils (
    cleanString,
    getWordCount,
    getMostUsedWords
) where

import qualified Data.Map as Map
import Data.List (sortBy)
import Data.Char (toLower)

cleanString :: String -> String
cleanString [] = []
cleanString (x:xs)
    | x `elem` ['A'..'Z'] = toLower x : cleanString xs
    | x `elem` ".,;:!?-\"\'" = cleanString xs
    | otherwise = x : cleanString xs

getWordCount :: String -> Int
getWordCount content = length $ words content

addWordsToMap :: Map.Map String Int -> [String] -> Map.Map String Int
addWordsToMap map [] = map
addWordsToMap map (word:words) = let
    count = Map.findWithDefault 0 word map
    in addWordsToMap (Map.insert word (count + 1) map) words

getMostUsedWords :: String -> [(String, Int)]
getMostUsedWords [] = []
getMostUsedWords content = let
    wordsList = words $ cleanString content
    map = Map.empty
    mapList = Map.toList (addWordsToMap map wordsList)
    sortedMapList = sortBy (\(_, a) (_, b) -> compare b a) mapList
    in take 10 sortedMapList