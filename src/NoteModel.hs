{-# LANGUAGE OverloadedStrings #-}

-- TODO: password protect notes
-- TODO: get info about a note: time, word count, most used words

module NoteModel (
    Note(..),
    getNoteFromDB,
    getAllNotesFromDB,
    saveNoteToDB,
    deleteNoteFromDB,
    replaceNoteInDB,
    shareNote,
    searchNotesByPhrase
) where

import Database.SQLite.Simple

data Note = Note {
    title :: String,
    content :: String,
    createdBy :: String
} deriving (Show, Eq)

instance FromRow Note where fromRow = Note <$> field <*> field <*> field
instance ToRow Note where toRow (Note title content createdBy) = toRow (title, content, createdBy)

getNoteFromDB :: String -> String -> IO (Maybe Note)
getNoteFromDB email title = do
    conn <- open "notes.db"
    r <- query conn "SELECT title, content, created_by from notes WHERE created_by = ? AND title = ?" (email, title) :: IO [Note]
    close conn
    return $ case r of
        [] -> Nothing
        (note:_) -> Just note

getAllNotesFromDB :: String -> IO [Note]
getAllNotesFromDB email = do
    conn <- open "notes.db"
    r <- query conn "SELECT title, content, created_by from notes WHERE created_by = ?" (Only email) :: IO [Note]
    close conn
    return r

saveNoteToDB :: Note -> IO Bool
saveNoteToDB (Note title content createdBy) = do
    conn <- open "notes.db"
    note <- getNoteFromDB createdBy title
    case note of
        Just _ -> return False
        Nothing -> do
            execute conn "INSERT OR IGNORE INTO notes (title, content, created_by) VALUES (?,?,?)" (Note title content createdBy)
            close conn
            return True

deleteNoteFromDB :: String -> String -> IO Bool
deleteNoteFromDB email title = do
    conn <- open "notes.db"
    note <- getNoteFromDB email title
    case note of
        Just _ -> do
            execute conn "DELETE FROM notes WHERE created_by = ? AND title = ?" (email, title)
            close conn
            return True
        Nothing -> return False

replaceNoteInDB :: Note -> IO Bool
replaceNoteInDB (Note title content createdBy) = do
    conn <- open "notes.db"
    note <- getNoteFromDB createdBy title
    case note of
        Just _ -> do
            execute conn "UPDATE notes SET content = ? WHERE created_by = ? AND title = ?" (content, createdBy, title)
            close conn
            return True
        Nothing -> return False


shareNote :: String -> String -> String -> IO Bool
shareNote email title emailToShare = do
    conn <- open "notes.db"
    note <- getNoteFromDB email title
    case note of
        Just note -> do
            let (Note title content _) = note
            execute conn "INSERT OR IGNORE INTO notes (title, content, created_by) VALUES (?,?,?)" (Note title content emailToShare)
            close conn
            return True
        Nothing -> return False

searchNotesByPhrase :: String -> String -> IO [Note]
searchNotesByPhrase email phrase = do
    conn <- open "notes.db"
    r <- query conn "SELECT title, content, created_by from notes WHERE created_by = ? AND content LIKE ?" (email, "%" ++ phrase ++ "%") :: IO [Note]
    close conn
    return r