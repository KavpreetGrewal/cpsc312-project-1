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
            username <- getUsername
            password <- getPassword
            user <- createUser username password
            noteTitle <- getNoteTitle
            noteContent <- getNoteContent
            createdBy <- getTime
            note <- createNote user noteTitle noteContent createdBy
            putStrLn "Note created!"
            notesApp
       {-  EditNote -> do
            username <- getUsername
            noteTitle <- editNoteTitle
            noteContent <- editNoteContent
            createdBy <- editTime
            note <- editNote username noteTitle noteContent createdBy
            putStrLn "Note edited!"
            notesApp -}


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

getUsername :: IO String
getUsername = do
    putStrLn "Enter your username:"
    getLine

getTime :: IO String 
getTime = do
  putStrLn "Enter the date"
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
    

editNoteContent :: IO String
editNoteContent = do
  putStrLn "Would you like to edit the content? (Enter Y for yes, N for no)"
  getResponse <- getLine
  if (response == "Y")
    then
      getNoteContent
      else if (response == "N")
        then
          ???

    

editUsername :: IO String
editUsername = do
    putStrLn "Enter your username:"
    getLine

editTime :: IO String 
editTime = do
  putStrLn "Enter the date"
  getLine

 -}