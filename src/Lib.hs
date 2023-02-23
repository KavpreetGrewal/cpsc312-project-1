module Lib (  
--  login,
--  logout,
  notesApp
) where

import Note (Note(..), createNote, getNoteTitle)
-- import qualified Data.Map as Map

--------------------------------------------------------------------------------------------------------------

-- noteS :: NoteS
-- noteS = "This is a note"

data MenuOption = CreateNote | EditNote | DeleteNote | Quit deriving (Show, Eq)

-- note :: Note
-- note = Note "math" "1+1=2"

notesApp :: IO ()
notesApp = do
    putStrLn "Welcome to the notes app!"
    menuOption <- getUserOption
    case menuOption of
        CreateNote -> do
            username <- getUsername
            password <- getPassword
            user <- createUser username password
            noteTitle <- getNoteTitle
            noteContent <- getNoteContent
            note <- createNote user noteTitle noteContent
            putStrLn "Note created!"
            notesApp

