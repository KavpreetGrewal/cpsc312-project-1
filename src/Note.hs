module Note
    ( Note
    , createNote
    , editNote
    ) where

import UserModel (User)
import NoteModel (Note(..), saveNoteToDB, getNoteFromDB, replaceNoteInDB)
import Data.List (find, delete)

createNote :: User -> String -> String -> String -> IO Note
createNote user title content createdBy = do 
    let note = Note title content createdBy
    -- save note to database 
    saveNoteToDB note
    return note

editNote :: String -> String -> String -> String -> IO Note
editNote username title content createdBy = do
    getNoteFromDB username title
    let note = Note title content createdBy            -- need to fix what actually happens to note
    replaceNoteInDB note
    return note


