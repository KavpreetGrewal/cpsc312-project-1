module Lib (  
--  login,
--  logout,
  notesApp
) where

import NoteModel (Note(..), getAllNotesFromDB, replaceNoteInDB, deleteNoteFromDB)
import Note (createNote)
import User (createUser)
import UserModel (User(..), getUserFromDB)
import Control.Exception (bracket_)
import System.IO (hFlush, stdout, stdin, hSetEcho, hGetEcho)
import Data.List (intercalate)
import Text.Read (readMaybe)
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
            email <- getEmail
            password <- getPassword
            user <- createUser email password
            noteTitle <- getNoteTitle
            noteContent <- getNoteContent
            createdBy <- getTime
            note <- createNote user noteTitle noteContent createdBy
            putStrLn "Note created!"
            notesApp
        EditNote -> do
            email <- getEmail                 
            user <- getUserFromDB email       -- Just needs email right?
            notes <- getAllNotesFromDB email  -- Just needs email right?
            noteIndex <- getNoteIndex notes
            let note = notes !! noteIndex
            newTitle <- editNoteTitle
            newContent <- editNoteContent
            dateModified <- getTime
            let updatedNote = note { title = newTitle, content = newContent, createdBy = dateModified}
            replaceNoteInDB updatedNote
            putStrLn "Note updated!"
            notesApp
        DeleteNote -> do
            email <- getEmail
            notes <- getAllNotesFromDB email
            noteIndex <- getNoteIndex notes
            let note = notes !! noteIndex
            noteTitle <- getNoteTitleFromNote note
            deleteNoteFromDB email noteTitle
            putStrLn "Note deleted!"
            notesApp
        Quit -> do
            putStrLn "You have quit the notes app"


getUserOption :: IO MenuOption
getUserOption = do
    putStrLn "What would you like to do?"
    putStrLn "1. Create a note"
    putStrLn "2. Edit a note"
    putStrLn "3. Delete a note"
    putStrLn "4. Quit"
    input <- getLine
    case input of
        "1" -> return CreateNote
        "2" -> return EditNote
        "3" -> return DeleteNote
        "4" -> return Quit
        _   -> do
            putStrLn "Invalid input, please try again."
            getUserOption

getPassword :: IO String
getPassword = do
    putStr "Enter password: "
    hFlush stdout
    pass <- withEcho False getLine
    putStrLn ""
    return pass

withEcho :: Bool -> IO a -> IO a
withEcho echo action = do
    old <- hGetEcho stdin
    bracket_ (hSetEcho stdin echo) (hSetEcho stdin old) action


getNoteTitle :: IO String
getNoteTitle = do
    putStrLn "Enter the note title:"
    getLine

getNoteTitleFromNote :: Note -> IO String
getNoteTitleFromNote note = return (title note)


getNoteContent :: IO String
getNoteContent = do
    putStrLn "Enter the note content:"
    getLine

getNoteIndex :: [Note] -> IO Int
getNoteIndex notes = do
    putStrLn "Select a note to edit or delete:"
    printNotes notes
    putStr "Enter note number: "
    input <- getLine
    case readMaybe input of
        Just index -> if index >= 0 && index <= length notes
            then return index
            else putStrLn "Invalid note number." >> getNoteIndex notes
        Nothing -> putStrLn "Invalid input. Enter a number." >> getNoteIndex notes

printNotes :: [Note] -> IO ()
printNotes notes = do
    putStrLn "Your notes:"
    mapM_ printNoteWithIndex (zip [0..] notes)

printNoteWithIndex :: (Int, Note) -> IO ()
printNoteWithIndex (index, note) = do
    putStrLn $ show index ++ ". " ++ title note
    putStrLn $ content note
    putStrLn ""

getEmail :: IO String
getEmail = do
    putStrLn "Please enter your email:"         
    getLine

getTime :: IO String 
getTime = do
  putStrLn "Enter today's date: "
  getLine

{- editNoteTitle :: IO String
editNoteTitle = do
    putStrLn "Would you like to edit the title? (Enter Y for yes, N for no)"
    response <- getLine
    if (response == "Y")
      then
        getNoteTitle
        else if (response == "N")
          then
            ???     
            -}   


findNoteTitle :: IO String
findNoteTitle = do
  putStrLn "Please enter the title of the note you would like to select: "
  getLine
    
editNoteTitle :: IO String
editNoteTitle = do
  putStrLn "Please enter the new title for your note: "
  getLine


editNoteContent :: IO String
editNoteContent = do
  putStrLn "Please enter the new content you would like to save for this note: "
  getLine
    

