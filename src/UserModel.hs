{-# LANGUAGE OverloadedStrings #-}

module UserModel (
    User(..),
    getUserFromDB,
    loginUserFromDB,
    saveUserToDB,
    deleteUserFromDB,
    updateUserInDB  
) where

import Database.SQLite.Simple

data User = User {
    email :: String,
    password :: String
} deriving (Show, Eq)

instance FromRow User where fromRow = User <$> field <*> field
instance ToRow User where toRow (User email password) = toRow (email, password)


getUserFromDB :: String -> IO (Maybe User)
getUserFromDB email = do
    conn <- open "notes.db"
    r <- query conn "SELECT * from users WHERE email = ?" (Only email) :: IO [User]
    close conn
    return $ case r of
        [] -> Nothing
        (user:_) -> Just user

loginUserFromDB :: String -> String -> IO (Maybe User)
loginUserFromDB email password= do
    conn <- open "notes.db"
    r <- query conn "SELECT * from users WHERE email = ? AND password = ?" (email, password) :: IO [User]
    close conn
    return $ case r of
        [] -> Nothing
        (user:_) -> Just user

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
