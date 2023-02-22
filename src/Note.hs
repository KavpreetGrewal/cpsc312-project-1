module Note
    ( NoteS, Note
    , createNote
    -- , getNotesForUser
    -- , updateNote
    -- , deleteNoteFromDb
    ) where

import UserModel (User)
import Data.List (find, delete)

type NoteS = String

data Note = Note 
    { title :: String
    , content :: String
    } deriving (Show, Eq)

createNote :: User -> String -> String -> IO Note
createNote user title content = do
    let note = Note title content
    -- save note to database (e.g. a Map)
    -- saveNote user note
    return note

-- Function needs to interact with database

-- A function to delete a note from user in the database based on the title 
-- deleteNoteFromDb :: User -> String -> IO ()
-- deleteNoteFromDb user noteTitle = do
--     let notes = Map.findWithDefault [] (username user) notesDb
--         updatedNotes = filter (\n -> title n /= noteTitle) notes
--         updatedDb = Map.insert (username user) updatedNotes notesDb
--     writeTVar notesDbVar updatedDb
