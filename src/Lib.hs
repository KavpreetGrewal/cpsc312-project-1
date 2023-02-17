module Lib (notesApp) where

import Note (Note)


note :: Note
note = "This is a note"

notesApp :: IO ()
notesApp = do
    putStrLn "Welcome to the notes app!"
    putStrLn note
