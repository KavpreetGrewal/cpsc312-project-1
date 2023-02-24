module Lib (  
--  login,
--  logout,
  notesApp
) where

import Note (Note(..), createNote)
import User (createUser)
import UserModel (User(..))
import Control.Exception (bracket_)
import System.IO (hFlush, stdout, stdin, hSetEcho, hGetEcho)
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
            user <- createUser username password
            noteTitle <- getNoteTitle
            noteContent <- getNoteContent
            createdBy <- getTime
            note <- createNote user noteTitle noteContent createdBy
            putStrLn "Note created!"
            notesApp
        EditNote -> do
            email <- findEmail                 
            noteTitle <- findNoteTitle
            newNoteTitle <- editNoteTitle
            newNoteContent <- editNoteContent
            dateModified <- getTime
            note <- editNote username noteTitle newNoteTitle newNoteContent dateModified
            putStrLn "Note edited!"
            notesApp 
        DeleteNote -> do
            email <- findEmail
            noteTitle <- findNoteTitle
            note <- deleteNote username noteTitle 
            putStrLn "Note deleted!"
            notesApp
        Quit -> do
            putStrLn "You have quit the notes app"
            return



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

getNoteContent :: IO String
getNoteContent = do
    putStrLn "Enter the note content:"
    getLine

getEmail :: IO String
getUsername = do
    putStrLn "Enter your email:"         
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
   

findEmail :: IO String 
findEmail = do
  putStrLn "Please enter your email"
  getLine

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
    

