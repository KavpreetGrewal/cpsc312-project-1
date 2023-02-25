module User (
    createUser,
    loginUser
) where

import Crypto.Hash (SHA256 (..), hash, Digest)
import qualified Data.ByteString.Char8 as BS

import UserModel (User(..), saveUserToDB, loginUserFromDB)


hashPassword :: String -> IO String
hashPassword password = do
    let bytestring = BS.pack password
    let hashedBytes = hash bytestring :: Digest SHA256
    return $ show hashedBytes

createUser :: String -> String -> IO (Maybe User)
createUser email password = do
    hashedPassword <- hashPassword password
    let user = User email hashedPassword
    res <- saveUserToDB user
    (if res then return $ Just user else return Nothing)

loginUser :: String -> String -> IO (Maybe User)
loginUser email password = do
    hashedPassword <- hashPassword password
    loginUserFromDB email hashedPassword