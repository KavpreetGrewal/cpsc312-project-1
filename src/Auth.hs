module Auth (
    loginOrRegisterUser,        -- loginOrRegisterUser :: IO User
    getEmail,                   -- getEmail :: IO String
    getPassword,                -- getPassword :: IO String
    withEcho                    -- withEcho :: Bool -> IO a -> IO a
) where

import Control.Exception (bracket_)
import System.Exit (exitSuccess)
import System.IO (hFlush, stdout, stdin, hSetEcho, hGetEcho)

import UserModel (User)
import User (createUser, loginUser)
import Utils (getUsernameFromEmail, checkEmailFormat, checkLength)


withEcho :: Bool -> IO a -> IO a
withEcho echo action = do
    old <- hGetEcho stdin
    bracket_ (hSetEcho stdin echo) (hSetEcho stdin old) action

{-
    Prompt user to enter their email
-}
getEmail :: IO String
getEmail = do
    putStrLn "Please enter your email:"         
    line <- getLine
    if checkLength line 5 320 && checkEmailFormat line
        then return line
        else putStrLn "The format of the email is not correct.\n" >> getEmail

{-
    Prompt user to enter their password
-}
getPassword :: IO String
getPassword = do
    putStr "Enter password: "
    hFlush stdout
    pass <- withEcho False getLine
    putStrLn ""
    return pass

{-
    Prompt user to login, register, quit. Users are not able to register one email with multiple accounts.
-}
loginOrRegisterUser :: IO User
loginOrRegisterUser = do
    putStrLn "Please choose an option:"
    putStrLn "1. Login  |  2. Register  |  3. Quit"
    input <- getLine
    case input of
        "1" -> do
            email <- getEmail
            password <- getPassword
            maybeUser <- loginUser email password
            case maybeUser of
                Just user -> putStrLn ("You are logged. Welcome " ++ getUsernameFromEmail email ++ "!\n") >> return user
                Nothing -> putStrLn "Invalid email or password.\n" >> loginOrRegisterUser
        "2" -> do
            email <- getEmail
            password <- getPassword
            user <- createUser email password
            case user of
                Nothing -> putStrLn "User already exists. Please login or use another email.\n" >> loginOrRegisterUser
                Just u -> putStrLn ("User created! Welcome " ++ getUsernameFromEmail email ++ "!\n") >> return u
        "3" -> do
            putStrLn "Have a nice day!"
            exitSuccess
        _ -> putStrLn "Invalid input.\n" >> loginOrRegisterUser
