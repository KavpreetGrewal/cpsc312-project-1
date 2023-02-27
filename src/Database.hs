{-# LANGUAGE OverloadedStrings #-}

module Database (
    setupDB,        -- setupDB :: IO ()
    resetDB         -- resetDB :: IO ()
) where

import Database.SQLite.Simple

{-
    Set up database to store notes created by users
-}
setupDB :: IO ()
setupDB = do
    conn <- open "notes.db"
    execute_ conn "CREATE TABLE IF NOT EXISTS users (email TEXT PRIMARY KEY, password TEXT)"
    execute_ conn "CREATE TABLE IF NOT EXISTS notes (\
        \id INTEGER PRIMARY KEY AUTOINCREMENT,\
        \title TEXT,\
        \content TEXT,\
        \created_by TEXT,\
        \date TEXT,\
        \UNIQUE (title, created_by),\
        \FOREIGN KEY (created_by) REFERENCES users (email) ON DELETE CASCADE ON UPDATE NO ACTION)"
    close conn

{-
    Clear all existing users and notes from the database
-}
resetDB :: IO ()
resetDB = do
    conn <- open "notes.db"
    execute_ conn "DROP TABLE IF EXISTS notes"
    execute_ conn "DROP TABLE IF EXISTS users"
    close conn
    setupDB