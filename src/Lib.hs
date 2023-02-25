module Lib (notesApp) where

import System.Exit (exitSuccess)
import Text.Read (readMaybe)

import Auth (loginOrRegisterUser)
import NoteModel (Note(..))
import Note (
    createNote,
    checkIfNoteExists,
    getListOfNotes,
    getNote,
    getNoteInfo,
    searchNotesByPhrase,
    editNote,
    deleteNote, shareNote)
import UserModel (User(..))

data MenuOption = CreateNote | EditNote | ViewNotes | SearchNotes | DeleteNote | ShareNote | Quit deriving (Show, Eq)

notesApp :: IO ()
notesApp = do
    putStrLn "Welcome to the notes app!"
    user <- loginOrRegisterUser
    mainMenu user

getUserOption :: IO MenuOption
getUserOption = do
    putStrLn "What would you like to do?"
    putStrLn 
        "1. Create a note  |  2. Edit a note  | 3. View notes  |  4. Search notes  |\
        \  5. Delete a note  | 6. Share a note |  7. Quit"
    input <- getLine
    case input of
        "1" -> return CreateNote
        "2" -> return EditNote
        "3" -> return ViewNotes
        "4" -> return SearchNotes
        "5" -> return DeleteNote
        "6" -> return ShareNote
        "7" -> return Quit
        _ -> do
            putStrLn "Invalid input, please try again."
            getUserOption

mainMenu :: User -> IO ()
mainMenu user = do
    menuOption <- getUserOption
    case menuOption of
        CreateNote -> createNoteOption user
        EditNote -> editNoteOption user
        ViewNotes -> viewNotesOption user
        SearchNotes -> searchNotesOption user
        DeleteNote -> deleteNoteOption user
        ShareNote -> shareNoteOption user
        Quit -> do
            putStrLn "You have quit the notes app"
            exitSuccess


{-
    Menu option handlers
-}
createNoteOption :: User -> IO ()
createNoteOption user@(User email _) = do
    noteTitle <- getNoteTitle email
    noteContent <- getNoteContent
    res <- createNote user noteTitle noteContent
    case res of
        Nothing -> putStrLn "Note could not be saved. Please try again." >> mainMenu user
        Just _ ->  putStrLn ("The note titled \"" ++ noteTitle ++ "\" has been created!\n") >> mainMenu user


editNoteOption :: User -> IO ()
editNoteOption user@(User email _) = do
    notes <- getListOfNotes email
    putStrLn "Select a note to edit (or q to go back):"
    printNotes notes
    noteIndex <- getNoteIndex notes

    if noteIndex == 0 then mainMenu user else do
        let title = notes !! (noteIndex - 1)
        note <- getNote email title
        case note of
            Nothing -> mainMenu user
            Just n@(Note _ content _) -> do
                printNote n
                newTitle <- editNoteTitle title
                newContent <- editNoteContent content
                res <- editNote title $ Note newTitle newContent email
                if res
                    then putStrLn "Note updated!" >> mainMenu user
                    else putStrLn "Note could not be updated. Please try again." >> editNoteOption user

viewNotesOption :: User -> IO ()
viewNotesOption user@(User email _) = do
    notes <- getListOfNotes email
    putStrLn "Select a note to view (or q to go back):"
    printNotes notes
    noteIndex <- getNoteIndex notes
    if noteIndex == 0
        then mainMenu user
        else do
            let title = notes !! (noteIndex - 1)
            (note, wordCount, topWords) <- getNoteInfo email title
            case note of
                Nothing -> putStrLn "Could not open note. Please try again!" >> viewNotesOption user
                Just n -> do
                    printNote n
                    printNoteInfo wordCount topWords
                    mainMenu user

searchNotesOption :: User -> IO ()
searchNotesOption user@(User email _) = do
    putStrLn "Enter a search term/phrase:"
    searchTerm <- getLine
    notes <- searchNotesByPhrase email searchTerm
    if null notes
        then putStrLn "No notes contain your search term." >> mainMenu user
        else do
            putStrLn "The following notes contain your search term:"
            printNotes notes
            mainMenu user

deleteNoteOption :: User -> IO ()
deleteNoteOption user@(User email _) = do
    notes <- getListOfNotes email
    putStrLn "Select a note to delete (or q to go back):"
    printNotes notes
    noteIndex <- getNoteIndex notes
    if noteIndex == 0
        then mainMenu user
        else do
            let title = notes !! (noteIndex - 1)
            res <- deleteNote email title
            if res
                then putStrLn "Note deleted!" >> mainMenu user
                else putStrLn "Note could not be deleted. Please try again." >> deleteNoteOption user

shareNoteOption :: User -> IO ()
shareNoteOption user@(User email _) = do
    notes <- getListOfNotes email
    putStrLn "Select a note to share (or q to go back):"
    printNotes notes
    noteIndex <- getNoteIndex notes
    if noteIndex == 0
        then mainMenu user
        else do
            let title = notes !! (noteIndex - 1)
            putStrLn "Enter the email address of the user you want to share the note with:"
            recipientEmail <- getLine
            res <- shareNote email title recipientEmail
            if res
                then putStrLn "Note shared!" >> mainMenu user
                else putStrLn "Note could not be shared. Please try again." >> shareNoteOption user


{- 
    IO helper functions
-}
getNoteTitle :: String -> IO String
getNoteTitle email = do
    putStrLn "Enter the note's title:"
    title <- getLine
    noteExists <- checkIfNoteExists email title
    if noteExists
        then putStrLn "A note with that title already exists. Please try again." >> getNoteTitle email
        else return title

getNoteContent :: IO String
getNoteContent = do
    putStrLn "Enter the note's content. To finish, type :q on a new line:"
    multiLineInput

multiLineInput :: IO String
multiLineInput = do
    input <- getLine
    if input == ":q"
        then return ""
        else do
            rest <- multiLineInput
            return $ input ++ " \n" ++ rest

printNote :: Note -> IO ()
printNote note = do
    putStrLn "-- Your note --"
    putStrLn $ "Title: " ++ title note
    putStrLn $ "Content: \n" ++ content note

printNotes :: [String] -> IO ()
printNotes notes = do
    mapM_ printNoteWithIndex (zip [1..] notes)

printNoteInfo :: Int -> [(String, Int)] -> IO ()
printNoteInfo wordCount topWords = do
    putStrLn "-- Note info --"
    putStrLn $ "Word count: " ++ show wordCount
    putStrLn "5 most used words:"
    mapM_ printWordWithCount topWords
    putStrLn ""

printWordWithCount :: (String, Int) -> IO ()
printWordWithCount (word, count) = putStrLn $ word ++ ": " ++ show count

getNoteIndex :: [String] -> IO Int
getNoteIndex notes = do
    putStrLn "Enter note number: "
    input <- getLine
    if input == "q"
        then return 0
        else case readMaybe input of
            Just index -> if index >= 1 && index <= length notes
                then return index
                else putStrLn "\nInvalid note number." >> getNoteIndex notes
            Nothing -> putStrLn "\nInvalid input. Please enter a number." >> getNoteIndex notes

printNoteWithIndex :: (Int, String) -> IO ()
printNoteWithIndex (index, note) = do
    putStrLn $ show index ++ ". " ++ note


editNoteTitle :: String -> IO String
editNoteTitle title = do
    putStrLn "Enter the new title (or press enter to keep the same title):"
    line <- getLine
    if line == ""
        then return title
        else return line

editNoteContent :: String -> IO String
editNoteContent content = do
    putStrLn "Enter the new content. To finish, type :q on a new line (or press enter to skip):"
    line <- getLine
    if line == ""
        then return content
        else do
            rest <- multiLineInput
            return $ line ++ " \n" ++ rest


