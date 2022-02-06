import System.Environment
import Control.Monad  
import Data.Char  
import Text.Read

--data ContentType = ContentHH | ContentSS | ContentPP deriving (Eq)
--instance Show ContentType where
--    show ContentHH = "HH"
--    show ContentSS = "$$"
--    show ContentPP = "%%"

data Tag = Main | Meta | History | Info | Exhibit | Biblio | Keywords | BibTeX | ToDo deriving (Eq)
instance Show Tag where
    show Main = ""
    show Meta = "$$"
    show History = "HH"
    show Info = "%%"
    show Exhibit = "EXH"
    show Biblio = "BB"
    show Keywords = "TT"
    show BibTeX = "BIBTEX"
    show ToDo = "TODO"

instance Read Tag where
    readsPrec _ "Meta" = [(Meta,"")]
    readsPrec _ "$$" = [(Meta,"")]
    readsPrec _ "HH" = [(History,"")]
    readsPrec _ "%%" = [(Info,"")]
    readsPrec _ "EXH" = [(Exhibit,"")]
    readsPrec _ "BB" = [(Biblio,"")]
    readsPrec _ "TT" = [(Keywords,"")]
    readsPrec _ "BIBTEX" = [(BibTeX,"")]
    readsPrec _ "TODO" = [(ToDo,"")]
    readsPrec _ t = error $ "Tag '" ++ t ++ "' not recognized."

data Command = SKIP
             | CHECK
             | GET { fileName :: String, tag :: Tag }
             | SET

argOrPrompt :: Int -> String -> IO String
argOrPrompt n message = do
    args <- getArgs
    if n < length args
        then return (args !! n)
        else (putStrLn (message ++ ":") >> getLine)

argOrErr :: Int -> String -> IO String
argOrErr n argName = do
    args <- getArgs
    if n < length args
        then return (args !! n)
        else error $
            "Expected at least " ++ (show $ n+1) ++ " args -- no value for '" ++ argName ++ "'!"


doCmd :: Command -> [String] -> IO ()
doCmd SKIP _ = return ()
doCmd CHECK l =  mapM putStrLn l >> putStrLn "Done!"
doCmd (GET {fileName=fN, tag=tG}) l = do
    putStrLn fN 
    putStrLn (show tG) 
    mapM putStrLn l
    putStrLn "Done!"


main = do
    cmd <- argOrErr 0 "cmd"
    args <- fmap tail getArgs
    case cmd of 
        "skip" -> doCmd SKIP []
        "check" -> doCmd CHECK args
        "get" -> do
                    in_fileName <- argOrErr 1 "fileName"
                    in_tag <- fmap read $ argOrErr 2 "tag"
                    doCmd (GET in_fileName in_tag) (drop 2 args)
        _ -> error $ "Command '" ++ cmd ++ "' not recognized."
