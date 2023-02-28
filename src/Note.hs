module Note (
    Note,
    createNote,                 -- createNote :: User -> String -> String -> IO (Maybe Note)
    checkIfNoteExists,          -- checkIfNoteExists :: String -> String -> IO Bool
    getListOfNotes,             -- getListOfNotes :: String -> IO [String]
    getNote,                    -- getNote :: String -> String -> IO (Maybe Note)
    getNoteInfo,                -- getNoteInfo :: String -> String -> IO (Maybe Note, Int, [(String, Int)])
    searchNotesByPhrase,        -- searchNotesByPhrase :: String -> String -> IO [String]
    editNote,                   -- editNote :: String -> Note -> IO Bool
    deleteNote,                 -- deleteNote :: String -> String -> IO Bool
    shareNote                   -- shareNote :: String -> String -> String -> IO Bool
) where

import NoteModel (
    Note(..),
    getNoteFromDB,              -- getNoteFromDB :: String -> String -> IO (Maybe Note)
    saveNoteToDB,               -- saveNoteToDB :: Note -> IO Bool
    getAllNotesFromDB,          -- getAllNotesFromDB :: String -> IO [Note]
    searchNotesByPhraseDB,      -- searchNotesByPhraseDB :: String -> String -> IO [Note]
    replaceNoteInDB,            -- replaceNoteInDB :: String -> Note -> IO ()
    deleteNoteFromDB,           -- deleteNoteFromDB :: String -> String -> IO ()
    transferNoteInDB)           -- transferNoteInDB :: Note -> String -> IO ()
import UserModel (User(..), getUserFromDB)
import Utils (getWordCount, getMostUsedWords, getTime)

{-
    Get a note from the database
-}
getNote :: String -> String -> IO (Maybe Note)
getNote = getNoteFromDB

{-
    Create a list of all notes (specifically their titles) in the database
-}
getListOfNotes :: String -> IO [String]
getListOfNotes email = do
    notes <- getAllNotesFromDB email
    let listOfNotes = map (\(Note title _ _ _) -> title) notes
    return listOfNotes

{-
    Check if a note exists in the database
-}
checkIfNoteExists :: String -> String -> IO Bool
checkIfNoteExists email title = do
    note <- getNoteFromDB email title
    case note of
        Nothing -> return False
        Just _ -> return True

{-
    Create a new note and save it to the database
-}
createNote :: User -> String -> String -> IO (Maybe Note)
createNote (User email _) title content = do
    time <- getTime
    let note = Note title content email time
    res <- saveNoteToDB note
    (if res then return $ Just note else return Nothing)

{-
    Retrieve a note from the database and edit its title and content. Time is updated to the time when the note edited
-}
editNote :: String -> Note -> IO Bool
editNote oldTitle (Note title content email _) = do
    note <- getNoteFromDB email oldTitle
    case note of
        Just _ -> do
            time <- getTime
            replaceNoteInDB oldTitle (Note title content email time)
            return True
        Nothing -> return False

{-
    Delete a note from the database
-}
deleteNote :: String -> String -> IO Bool
deleteNote email title = do
    note <- getNoteFromDB email title
    case note of
        Just _ -> do
            deleteNoteFromDB email title
            return True
        Nothing -> return False

{-
    Gets the word count and most used word with its frequency for a note
-}
getNoteInfo :: String -> String -> IO (Maybe Note, Int, [(String, Int)])
getNoteInfo email title = do
    note <- getNoteFromDB email title
    case note of
        Nothing -> return (Nothing, 0, [])
        Just n@(Note _ content _ _) -> do
            let wordCount = getWordCount content
            let mostUsedWords = getMostUsedWords content
            return (Just n, wordCount, mostUsedWords)

{-
    Search all notes in the database for a particular word or phrase and return a list of all notes containing that word or phrase
-}
searchNotesByPhrase :: String -> String -> IO [String]
searchNotesByPhrase email phrase = do
    notes <- searchNotesByPhraseDB email phrase
    let listOfNotes = map (\(Note title _ _ _) -> title) notes
    return listOfNotes

{-
    Retrieve a note from the database and share it to the email of the recipient
-}
shareNote :: String -> String -> String -> IO Bool
shareNote email title recepientEmail = do
    note <- getNoteFromDB email title
    if email == recepientEmail then return False
    else case note of
        Just (Note title' content createdBy date) -> do
            user <- getUserFromDB recepientEmail
            case user of
                Nothing -> return False
                Just _ -> do
                    let newTitle = title' ++ " (shared by " ++ email ++ ")"
                    transferNoteInDB (Note newTitle content createdBy date) recepientEmail
                    return True
        Nothing -> return False
