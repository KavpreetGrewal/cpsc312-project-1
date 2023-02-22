import Database (setupDB)
import Lib (notesApp)

main :: IO ()
main = do
    setupDB
    notesApp
