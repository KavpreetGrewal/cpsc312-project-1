{-# LANGUAGE OverloadedStrings #-}

module UserModel (
    User(..),
    getUserFromDB,          -- getUserFromDB :: String -> IO (Maybe User)
    loginUserFromDB,        -- loginUserFromDB :: String -> String -> IO (Maybe User)
    saveUserToDB,           -- saveUserToDB :: User -> IO Bool
    deleteUserFromDB,       -- deleteUserFromDB :: String -> IO Bool
    updateUserInDB          -- updateUserInDB :: User -> IO Bool
) where

import Database.SQLite.Simple

{-
    Create User data type
-}
data User = User {
    email :: String,
    password :: String
} deriving (Show, Eq)

instance FromRow User where fromRow = User <$> field <*> field
instance ToRow User where toRow (User email password) = toRow (email, password)

{-
    Retrieve a user from the database
-}
getUserFromDB :: String -> IO (Maybe User)
getUserFromDB email = do
    conn <- open "notes.db"
    r <- query conn "SELECT * from users WHERE email = ?" (Only email) :: IO [User]
    close conn
    return $ case r of
        [] -> Nothing
        (user:_) -> Just user

{-
    Select a user from the database matching a particular login
-}
loginUserFromDB :: String -> String -> IO (Maybe User)
loginUserFromDB email password= do
    conn <- open "notes.db"
    r <- query conn "SELECT * from users WHERE email = ? AND password = ?" (email, password) :: IO [User]
    close conn
    return $ case r of
        [] -> Nothing
        (user:_) -> Just user

{-
    Insert a user into the database
-}
saveUserToDB :: User -> IO Bool
saveUserToDB (User email password) = do
    conn <- open "notes.db"
    user <- getUserFromDB email
    case user of
        Just _ -> return False
        Nothing -> do
            execute conn "INSERT OR IGNORE INTO users (email, password) VALUES (?,?)" (User email password)
            close conn
            return True

{-
    Delete a user from the database
-}
deleteUserFromDB :: String -> IO Bool
deleteUserFromDB email = do
    conn <- open "notes.db"
    user <- getUserFromDB email
    case user of
        Just _ -> do
            execute conn "DELETE FROM users WHERE email = ?" (Only email)
            close conn
            return True
        Nothing -> return False

{-
    Update a user in the database with a new email and password
-}
updateUserInDB :: User -> IO Bool
updateUserInDB (User email password) = do
    conn <- open "notes.db"
    user <- getUserFromDB email
    case user of
        Just _ -> do
            execute conn "UPDATE users SET password = ? WHERE email = ?" (password, email)
            close conn
            return True
        Nothing -> return False
