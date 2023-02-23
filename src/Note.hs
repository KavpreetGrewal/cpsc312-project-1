module Note
    ( Note
    , createNote
    ) where

import UserModel (User)
import NoteModel (Note(..), saveNoteToDB)
import Data.List (find, delete)

createNote :: User -> String -> String -> String -> IO Note
createNote user title content createdBy = do 
    let note = Note title content createdBy
    -- save note to database 
    saveNoteToDB note
    return note

