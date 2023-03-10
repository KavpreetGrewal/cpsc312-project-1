module Lib (notesApp) where

import System.Exit (exitSuccess)
import Text.Read (readMaybe)

import Auth (loginOrRegisterUser)
import NoteModel (Note(..))
import Note (
    createNote,                 -- createNote :: User -> String -> String -> IO (Maybe Note)
    checkIfNoteExists,          -- checkIfNoteExists :: String -> String -> IO Bool 
    getListOfNotes,             -- getListOfNotes :: String -> IO [String]
    getNote,                    -- getNote :: String -> String -> IO (Maybe Note)
    getNoteInfo,                -- getNoteInfo :: String -> String -> IO (Maybe Note, Int, [(String, Int)])
    searchNotesByPhrase,        -- searchNotesByPhrase :: String -> String -> IO [String]
    editNote,                   -- editNote :: String -> Note -> IO Bool
    deleteNote,                 -- deleteNote :: String -> String -> IO Bool
    shareNote)                  -- shareNote :: String -> String -> String -> IO Bool
import UserModel (User(..))

data MenuOption = CreateNote | EditNote | ViewNotes | SearchNotes | DeleteNote | ShareNote | Quit deriving (Show, Eq)

notesApp :: IO ()
notesApp = do
    putStrLn "Welcome to the notes app!"
    user <- loginOrRegisterUser
    mainMenu user

{-
    Present user with options available in the notes app: create, edit, view, search, delete, share, and quit
-}
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

{-
    Read user input
-}
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

{- 
    Handler to edit a note. User can select an exisitng note associated with their email by title and 
    edit either the title or content of the note. 
-}
editNoteOption :: User -> IO ()
editNoteOption user@(User email _) = do
    notes <- getListOfNotes email
    putStrLn "Select a note to edit:"
    printNotes notes
    noteIndex <- getNoteIndex notes

    if noteIndex == 0 then mainMenu user else do
        let title = notes !! (noteIndex - 1)
        note <- getNote email title
        case note of
            Nothing -> mainMenu user
            Just n@(Note _ content _ _) -> do
                printNote n
                newTitle <- editNoteTitle title
                newContent <- editNoteContent content
                res <- editNote title $ Note newTitle newContent email ""
                if res
                    then putStrLn "Note updated!" >> mainMenu user
                    else putStrLn "Note could not be updated. Please try again." >> editNoteOption user

{- 
    Handler for option to view notes. User can input a title associated with their email and that note will be printed 
-}
viewNotesOption :: User -> IO ()
viewNotesOption user@(User email _) = do
    notes <- getListOfNotes email
    putStrLn "Select a note to view:"
    printNotes notes
    noteIndex <- getNoteIndex notes
    if noteIndex == 0
        then mainMenu user
        else do
            let title = notes !! (noteIndex - 1)
            (note, wordCount, topWords) <- getNoteInfo email title
            case note of
                Nothing -> putStrLn "Could not open note. Please try again!" >> viewNotesOption user
                Just n@(Note _ _ _ date) -> do
                    printNote n
                    printNoteInfo wordCount topWords date
                    mainMenu user

{-
    Handler for option to search notes. User can input a term or phrase and any notes containing that not
    or phrase witll be printed
-}
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

{-
    Handler for delete note option. Allows user to input title of note they would like deleted. If a note with 
    that title is associated with their email it will be deleted
-}
deleteNoteOption :: User -> IO ()
deleteNoteOption user@(User email _) = do
    notes <- getListOfNotes email
    putStrLn "Select a note to delete:"
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

{-
    Handler for share notes option. Allows a user to select a note associated with their email by title and then 
    input a different email address they would like to share the note to. If the other email also exists in the database, 
    the note will be shared.
-}
shareNoteOption :: User -> IO ()
shareNoteOption user@(User email _) = do
    notes <- getListOfNotes email
    putStrLn "Select a note to share:"
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

{-
    Allow the user to enter the title for a note.  The user is not able to create multiple notes with 
    the same title.
-}
getNoteTitle :: String -> IO String
getNoteTitle email = do
    putStrLn "Enter the note's title:"
    title <- getLine
    noteExists <- checkIfNoteExists email title
    if noteExists
        then putStrLn "A note with that title already exists. Please try again." >> getNoteTitle email
        else return title

{-
    Allow user to enter content for the note
-}
getNoteContent :: IO String
getNoteContent = do
    putStrLn "Enter the note's content. To finish, type :q on a new line:"
    multiLineInput


{-
    Allow user to enter multiple lines of text. The user will write :q to indicate they are done writing content
-}
multiLineInput :: IO String
multiLineInput = do
    input <- getLine
    if input == ":q"
        then return ""
        else do
            rest <- multiLineInput
            return $ input ++ " \n" ++ rest

{-
    Print the title and content of a note
-}
printNote :: Note -> IO ()
printNote note = do
    putStrLn "-- Your note --"
    putStrLn $ "Title: " ++ title note
    putStrLn $ "Content: \n" ++ content note

{-
    Prints multiple notes
-}
printNotes :: [String] -> IO ()
printNotes notes = do
    mapM_ printNoteWithIndex (zip [1..] notes)

{-
    Prints the information associated with a note including the date it was last modified, the word count, 
    and the five most used words in the note.
-}
printNoteInfo :: Int -> [(String, Int)] -> String -> IO ()
printNoteInfo wordCount topWords date = do
    putStrLn "-- Note info --"
    putStrLn $ "Date last modified: " ++ date
    putStrLn $ "Word count: " ++ show wordCount
    putStrLn "5 most used words:"
    mapM_ printWordWithCount topWords
    putStrLn ""

{-
    Print the number of times a word was used
-}
printWordWithCount :: (String, Int) -> IO ()
printWordWithCount (word, count) = putStrLn $ word ++ ": " ++ show count

{-
    Allow user to enter the index of the note they would like to select
-}
getNoteIndex :: [String] -> IO Int
getNoteIndex notes = do
    putStrLn "Enter the note's number (or q to go back): "
    input <- getLine
    if input == "q"
        then return 0
        else case readMaybe input of
            Just index -> if index >= 1 && index <= length notes
                then return index
                else putStrLn "\nInvalid note number." >> getNoteIndex notes
            Nothing -> putStrLn "\nInvalid input. Please enter a number." >> getNoteIndex notes

{-
    Print the note and it's index
-}
printNoteWithIndex :: (Int, String) -> IO ()
printNoteWithIndex (index, note) = do
    putStrLn $ show index ++ ". " ++ note

{-
    Allow user to enter a new title for an existing note
-}
editNoteTitle :: String -> IO String
editNoteTitle title = do
    putStrLn "Enter the new title (or press enter to keep the same title):"
    line <- getLine
    if line == ""
        then return title
        else return line

{-
    Allow user to enter new content for an existing note
-}
editNoteContent :: String -> IO String
editNoteContent content = do
    putStrLn "Enter the new content. To finish, type :q on a new line (or press enter to skip):"
    line <- getLine
    if line == ""
        then return content
        else do
            rest <- multiLineInput
            return $ line ++ " \n" ++ rest
