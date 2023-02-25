module Note (
    Note,
    createNote,
    checkIfNoteExists,
    getListOfNotes,
    getNote,
    getNoteInfo,
    searchNotesByPhrase,
    editNote,
    deleteNote,
    shareNote
) where

import NoteModel (Note(..), getNoteFromDB, saveNoteToDB, getAllNotesFromDB, searchNotesByPhraseDB, replaceNoteInDB, deleteNoteFromDB, transferNoteInDB)
import UserModel (User(..))
import Utils (getWordCount, getMostUsedWords)


getNote :: String -> String -> IO (Maybe Note)
getNote = getNoteFromDB

getListOfNotes :: String -> IO [String]
getListOfNotes email = do
    notes <- getAllNotesFromDB email
    let listOfNotes = map (\(Note title _ _) -> title) notes
    return listOfNotes

checkIfNoteExists :: String -> String -> IO Bool
checkIfNoteExists email title = do
    note <- getNoteFromDB email title
    case note of
        Nothing -> return False
        Just _ -> return True

createNote :: User -> String -> String -> IO (Maybe Note)
createNote (User email _) title content = do
    let note = Note title content email
    res <- saveNoteToDB note
    (if res then return $ Just note else return Nothing)

editNote :: String -> Note -> IO Bool
editNote oldTitle (Note title content email) = do
    note <- getNoteFromDB email oldTitle
    case note of
        Just _ -> do
            replaceNoteInDB oldTitle (Note title content email)
            return True
        Nothing -> return False

deleteNote :: String -> String -> IO Bool
deleteNote email title = do
    note <- getNoteFromDB email title
    case note of
        Just _ -> do
            deleteNoteFromDB email title
            return True
        Nothing -> return False

getNoteInfo :: String -> String -> IO (Maybe Note, Int, [(String, Int)])
getNoteInfo email title = do
    note <- getNoteFromDB email title
    case note of
        Nothing -> return (Nothing, 0, [])
        Just n@(Note _ content _) -> do
            let wordCount = getWordCount content
            let mostUsedWords = getMostUsedWords content
            return (Just n, wordCount, mostUsedWords)

searchNotesByPhrase :: String -> String -> IO [String]
searchNotesByPhrase email phrase = do
    notes <- searchNotesByPhraseDB email phrase
    let listOfNotes = map (\(Note title _ _) -> title) notes
    return listOfNotes

shareNote :: String -> String -> String -> IO Bool
shareNote email title recepientEmail = do
    note <- getNoteFromDB email title
    case note of
        Just note -> do
            transferNoteInDB note recepientEmail
            return True
        Nothing -> return False
