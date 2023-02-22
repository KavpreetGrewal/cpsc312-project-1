module Lib (  
--  login,
--  logout,
  notesApp
) where

import Note (NoteS, Note)
-- import qualified Data.Map as Map

-- login :: String -> String -> IO (Maybe User)
-- login username password = do
--     -- fetch user from database
--     maybeUser <- getUser username
--     case maybeUser of
--         Just user -> do
--             let hashedPassword = password user
--             if verifyPassword hashedPassword password
--                 then return $ Just user
--                 else return Nothing
--         Nothing -> return Nothing

--------------------------------------------------------------------------------------------------------------

noteS :: NoteS
noteS = "This is a note"

-- note :: Note
-- note = Note "math" "1+1=2"

notesApp :: IO ()
notesApp = do
    putStrLn "Welcome to the notes app!"
    putStrLn noteS

