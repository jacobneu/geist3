import System.Environment
import Control.Monad  
import Data.Char  
import Text.Read
import Nanoparsec

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
    readsPrec _ "meta" = [(Meta,"")]
    readsPrec _ "$$" = [(Meta,"")]
    readsPrec _ "HH" = [(History,"")]
    readsPrec _ "%%" = [(Info,"")]
    readsPrec _ "EXH" = [(Exhibit,"")]
    readsPrec _ "BB" = [(Biblio,"")]
    readsPrec _ "TT" = [(Keywords,"")]
    readsPrec _ "BIBTEX" = [(BibTeX,"")]
    readsPrec _ "TODO" = [(ToDo,"")]
    readsPrec _ "history" = [(History,"")]
    readsPrec _ "info" = [(Info,"")]
    readsPrec _ "exhibits" = [(Exhibit,"")]
    readsPrec _ "exhibit" = [(Exhibit,"")]
    readsPrec _ "exh" = [(Exhibit,"")]
    readsPrec _ "biblio" = [(Biblio,"")]
    readsPrec _ "bib" = [(Biblio,"")]
    readsPrec _ "keywords" = [(Keywords,"")]
    readsPrec _ "tags" = [(Keywords,"")]
    readsPrec _ "bibtex" = [(BibTeX,"")]
    readsPrec _ "bibTeX" = [(BibTeX,"")]
    readsPrec _ "BibTeX" = [(BibTeX,"")]
    readsPrec _ "ToDo" = [(ToDo,"")]
    readsPrec _ "todo" = [(ToDo,"")]
    readsPrec _ t = error $ "[FAIL] Tag '" ++ t ++ "' not recognized."

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
            "[FAIL] Expected at least " ++ (show $ n+1) ++ " args -- no value for '" ++ argName ++ "'!"

data Command = SKIP
             | CHECK
             | GET { fileName :: String, tag :: Tag }
             | SET

data Datatype = Plain String
              | KVList [(String,String)]

arentSpaces :: String -> Bool
arentSpaces = (foldl (||) False) . (map (not . isSpace)) 

kvListParser :: Parser [(String,String)]
kvListParser = do
    content <- consume
    let contWords = map words $ filter arentSpaces $ lines content
    return [(head l, unwords $ tail l) | l <- contWords] 

getParser :: Tag -> Parser Datatype
getParser Meta = fmap KVList $ extract1 ("[" ++ (show Meta) ++ "]") kvListParser
getParser tt = fmap Plain $ extract1 ("[" ++ (show tt) ++ "]") consume


indent :: String
indent = "    "

doCmd :: Command -> [String] -> IO ()
doCmd SKIP _ = return ()
doCmd CHECK l =  mapM putStrLn l >> putStrLn "Done!"
doCmd (GET {fileName=fN, tag=tG}) l = do
    fileContents <- readFile fN
    let desired = runParser (getParser tG) fileContents  
    case desired of 
        [] -> putStrLn $ "[FAIL] " ++ fN ++ " has no entry for " ++ (show tG)
        (Plain res:_) -> mapM_ putStrLn $ dropWhile (=="") (lines res)
        (KVList res:_) -> mapM_ putStrLn $ map (\(key,value) -> indent ++ key ++ " " ++ value) res
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
