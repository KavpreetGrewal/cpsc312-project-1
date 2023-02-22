module Lib ( createUser  
          --  , login
          --  , logout
           , notesApp
           ) where

import Note (NoteS, Note)
-- import qualified Data.Map as Map
import Crypto.Hash (SHA256 (..), hash, Digest)
import qualified Data.ByteString.Char8 as BS


data User = User
-- This represents a user who has created an account 
-- A user consists of a username and password
    { username :: String
    , password :: String
    } deriving (Show, Eq)


hashPassword :: String -> IO String
hashPassword password = do
  let passwordBytes = (BS.pack password) 
  let hashedBytes = hash passwordBytes :: Digest SHA256
  return $ show hashedBytes

createUser :: String -> String -> IO User

createUser username password = do
    hashedPassword <- hashPassword password
    let user = User username hashedPassword
    -- save user to database
    -- saveUser user -- Database function needs to be implemented later
    return user

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

