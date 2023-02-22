module User (
    createUser
) where

import Crypto.Hash (SHA256 (..), hash, Digest)
import qualified Data.ByteString.Char8 as BS

import UserModel (User(..), saveUserToDB)


hashPassword :: String -> IO String
hashPassword password = do
    let bytestring = BS.pack password
    let hashedBytes = hash bytestring :: Digest SHA256
    return $ show hashedBytes

createUser :: String -> String -> IO User
createUser username password = do
    hashedPassword <- hashPassword password
    let user = User username hashedPassword
    saveUserToDB user
    return user