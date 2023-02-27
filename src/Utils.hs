module Utils (
    cleanString,
    getWordCount,
    getMostUsedWords,
    getUsernameFromEmail,
    checkEmailFormat,
    checkLength,
    getTime
) where

import qualified Data.Map as Map
import Data.List (sortBy)
import Data.Char (toLower)
import Data.Time.Clock (getCurrentTime)

cleanString :: String -> String
cleanString [] = []
cleanString (x:xs)
    | x `elem` ['A'..'Z'] = toLower x : cleanString xs
    | x `elem` ".,;:!?-\"\'" = cleanString xs
    | otherwise = x : cleanString xs

getWordCount :: String -> Int
getWordCount content = length $ words content

addWordsToMap :: Map.Map String Int -> [String] -> Map.Map String Int
addWordsToMap m [] = m
addWordsToMap m (word:lst) = let
    count = Map.findWithDefault 0 word m
    in addWordsToMap (Map.insert word (count + 1) m) lst

getMostUsedWords :: String -> [(String, Int)]
getMostUsedWords [] = []
getMostUsedWords content = let
    wordsList = words $ cleanString content
    m = Map.empty
    mapList = Map.toList (addWordsToMap m wordsList)
    sortedMapList = sortBy (\(_, a) (_, b) -> compare b a) mapList
    in take 5 sortedMapList

getUsernameFromEmail :: String -> String
getUsernameFromEmail email = takeWhile (/= '@') email

checkEmailFormat :: String -> Bool
checkEmailFormat email = '@' `elem` email

checkLength :: String -> Int -> Int -> Bool
checkLength string minL maxL = len <= maxL && len >= minL
    where len = length string

getTime :: IO String
getTime = do
    time <- getCurrentTime
    return $ take 19 $ show time
