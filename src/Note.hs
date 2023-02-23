module Note
    ( Note
    , createNote
    ) where

import UserModel (User)
import NoteModel (saveNoteToDB)
import Data.List (find, delete)


data Note = Note 
    { title :: String
    , content :: String
    } deriving (Show, Eq)

createNote :: User -> String -> String -> IO Note
createNote user title content = do
    let note = Note title content
    -- save note to database 
    saveNoteToDB note
    return note


getNoteTitle :: IO String
getNoteTitle = do
    putStrLn "Enter the note title:"
    getLine
