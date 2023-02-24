module Note (
    Note,
    createNote
) where

import NoteModel (Note(..), getNoteFromDB, saveNoteToDB)
import UserModel (User(..))
import Utils (getWordCount, getMostUsedWords)

import Data.List (find, delete)

createNote :: User -> String -> String -> String -> IO Note
createNote user title content createdBy = do 
    let note = Note title content createdBy
    -- save note to database 
    saveNoteToDB note
    return note

getNoteInfo :: String -> String -> IO (Int, [(String, Int)])
getNoteInfo email title = do
    note <- getNoteFromDB email title
    case note of
        Nothing -> return (0, [])
        Just (Note _ content _) -> do
            let wordCount = getWordCount content
            let mostUsedWords = getMostUsedWords content
            return (wordCount, mostUsedWords)


editNote :: String -> String -> String -> String -> String -> IO Note
editNote username title newTitle newContent dateModified = do
    note <- getNoteFromDB username title
    note <- Note newTitle newContent dateModified
    replaceNoteInDB note
    return note

deleteNote :: String -> String -> IO Bool
deleteNote email title = do
    deleteNoteFromDB email title
    return
