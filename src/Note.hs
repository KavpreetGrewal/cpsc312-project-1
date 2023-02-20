module Note
    ( Note
    , createNote
    , getNotesForUser
    , updateNote
    , deleteNote
    ) where

-- type Note = String

data Note = Note
    { title :: String
    , content :: String
    } deriving (Show, Eq)