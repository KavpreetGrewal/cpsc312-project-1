module Utils (
    cleanString,                -- cleanString :: String -> String
    getWordCount,               -- getWordCount :: String -> Int
    getMostUsedWords,           -- getMostUsedWords :: String -> [(String, Int)]
    getUsernameFromEmail,       -- getUsernameFromEmail :: String -> String
    checkEmailFormat,           -- checkEmailFormat :: String -> Bool
    checkLength,                -- checkLength :: String -> Int -> Int -> Bool
    getTime                     -- getTime :: IO String
) where

import qualified Data.Map as Map
import Data.List (sortBy)
import Data.Char (toLower)
import Data.Time.Clock (getCurrentTime)

{-
    Take a string and make all letters lowercase and remove special characters
-}
cleanString :: String -> String
cleanString [] = []
cleanString (x:xs)
    | x `elem` ['A'..'Z'] = toLower x : cleanString xs
    | x `elem` ".,;:!?-\"\'" = cleanString xs
    | otherwise = x : cleanString xs

{-
    Function to count the number of words in the content of a note
-}
getWordCount :: String -> Int
getWordCount content = length $ words content

{-
    Add all words to a map
-}
addWordsToMap :: Map.Map String Int -> [String] -> Map.Map String Int
addWordsToMap m [] = m
addWordsToMap m (word:lst) = let
    count = Map.findWithDefault 0 word m
    in addWordsToMap (Map.insert word (count + 1) m) lst

{-
    Find the five most used words
-}
getMostUsedWords :: String -> [(String, Int)]
getMostUsedWords [] = []
getMostUsedWords content = let
    wordsList = words $ cleanString content
    m = Map.empty
    mapList = Map.toList (addWordsToMap m wordsList)
    sortedMapList = sortBy (\(_, a) (_, b) -> compare b a) mapList
    in take 5 sortedMapList

{-
    Make a username out of all characters before the @ in an email
-}
getUsernameFromEmail :: String -> String
getUsernameFromEmail email = takeWhile (/= '@') email

{-
    Check that an email contains the "@" symbol
-}
checkEmailFormat :: String -> Bool
checkEmailFormat email = '@' `elem` email

{-
    Check that a string is not under the minimum length or over the maximum length
-}
checkLength :: String -> Int -> Int -> Bool
checkLength string minL maxL = len <= maxL && len >= minL
    where len = length string

{-
    Get the current time
-}
getTime :: IO String
getTime = do
    time <- getCurrentTime
    return $ take 19 $ show time
