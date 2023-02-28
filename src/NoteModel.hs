{-# LANGUAGE OverloadedStrings #-}

module NoteModel (
    Note(..),
    getNoteFromDB,              -- getNoteFromDB :: String -> String -> IO (Maybe Note)
    getAllNotesFromDB,          -- getAllNotesFromDB :: String -> IO [Note]
    saveNoteToDB,               -- saveNoteToDB :: Note -> IO Bool
    deleteNoteFromDB,           -- deleteNoteFromDB :: String -> String -> IO ()
    replaceNoteInDB,            -- replaceNoteInDB :: String -> Note -> IO ()
    transferNoteInDB,           -- transferNoteInDB :: Note -> String -> IO ()
    searchNotesByPhraseDB       -- searchNotesByPhraseDB :: String -> String -> IO [Note]
) where

import Database.SQLite.Simple

{-
    A note data type. This is the data type that is used to represent a note in the database.
    Consists of a title, content, the email of the user who created it, and the date it was created.
-}
data Note = Note {
    title :: String,
    content :: String,
    createdBy :: String,
    date :: String
} deriving (Show, Eq)

instance FromRow Note where fromRow = Note <$> field <*> field <*> field <*> field
instance ToRow Note where toRow (Note title content createdBy date) = toRow (title, content, createdBy, date)

{-
    Retrieve a note from the database
-}
getNoteFromDB :: String -> String -> IO (Maybe Note)
getNoteFromDB email title = do
    conn <- open "notes.db"
    r <- query conn "SELECT title, content, created_by, date from notes WHERE created_by = ? AND title = ?" (email, title) :: IO [Note]
    close conn
    return $ case r of
        [] -> Nothing
        (note:_) -> Just note

{-
    Retrieve all notes from the database
-}
getAllNotesFromDB :: String -> IO [Note]
getAllNotesFromDB email = do
    conn <- open "notes.db"
    r <- query conn "SELECT title, content, created_by, date from notes WHERE created_by = ?" (Only email) :: IO [Note]
    close conn
    return r

{-
    Save a note to the database
-}
saveNoteToDB :: Note -> IO Bool
saveNoteToDB (Note title content createdBy date) = do
    conn <- open "notes.db"
    note <- getNoteFromDB createdBy title
    case note of
        Just _ -> return False
        Nothing -> do
            execute conn 
                "INSERT OR IGNORE INTO notes (title, content, created_by, date) VALUES (?,?,?,?)"
                (Note title content createdBy date)
            close conn
            return True

{-
    Delete a note from the database
-}
deleteNoteFromDB :: String -> String -> IO ()
deleteNoteFromDB email title = do
    conn <- open "notes.db"
    execute conn "DELETE FROM notes WHERE created_by = ? AND title = ?" (email, title)
    close conn

{-
    Replace an exisiting note in the database with a new (edited) note
-}
replaceNoteInDB :: String -> Note -> IO ()
replaceNoteInDB oldTitle (Note title content createdBy date) = do
    conn <- open "notes.db"
    execute conn 
        "UPDATE notes SET title = ?, content = ?, date = ? WHERE created_by = ? AND title = ?" 
        (title, content, date, createdBy, oldTitle)
    close conn

{-
    Transfer a note in the database from one user to another in the database
-}
transferNoteInDB :: Note -> String -> IO ()
transferNoteInDB (Note title content _ date) recepientEmail = do
    conn <- open "notes.db"
    execute conn
        "INSERT OR IGNORE INTO notes (title, content, created_by, date) VALUES (?,?,?,?)"
        (Note title content recepientEmail date)
    close conn

{-
    Select all note in the database containing a particular phrase 
-}
searchNotesByPhraseDB :: String -> String -> IO [Note]
searchNotesByPhraseDB email phrase = do
    conn <- open "notes.db"
    r <- query conn 
        "SELECT title, content, created_by, date from notes WHERE created_by = ? AND content LIKE ?"
        (email, "%" ++ phrase ++ "%") :: IO [Note]
    close conn
    return r
