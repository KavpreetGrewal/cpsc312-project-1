{-# LANGUAGE OverloadedStrings #-}

module Database (
    setupDB,
    resetDB
) where

import Database.SQLite.Simple

setupDB :: IO ()
setupDB = do
    conn <- open "notes.db"
    execute_ conn "CREATE TABLE IF NOT EXISTS users (email TEXT PRIMARY KEY, password TEXT)"
    execute_ conn "CREATE TABLE IF NOT EXISTS notes (\
        \id INTEGER PRIMARY KEY AUTOINCREMENT,\
        \title TEXT,\
        \content TEXT,\
        \created_by TEXT,\
        \UNIQUE (title, created_by),\
        \FOREIGN KEY (created_by) REFERENCES users (email) ON DELETE CASCADE ON UPDATE NO ACTION)"
    close conn

resetDB :: IO ()
resetDB = do
    conn <- open "notes.db"
    execute_ conn "DELETE FROM notes"
    execute_ conn "DELETE FROM users"
    close conn
    setupDB