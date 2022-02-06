import System.IO
import System.Directory
import System.Environment
import Data.List

indent = "    "
splitComma :: String -> [String]
splitComma s =  case dropWhile (==',') s of
                      "" -> []
                      s' -> w : splitComma s''
                            where (w, s'') = break (==',') s'

argOrPrompt :: Int -> String -> IO String
argOrPrompt n message = do
    args <- getArgs
    if n < length args
        then return (args !! n)
        else (putStrLn (message ++ ":") >> getLine)

toHead :: (a -> a) -> [a] -> [a]
toHead f (x:xs) = f x : xs

spaceNn :: String -> String
spaceNn "\n" = "\n"
spaceNn s = s ++ " "

{- If tag=currentStr, then flip resBit, else leave alone
 - Add currentStr to resList if resBit true
 -}
selector :: String -> String -> (Bool,[String]) -> (Bool,[String])
selector tag currentStr (resBit,resList) = case (resBit, tag == currentStr) of
    (True,True) -> (False, resList)
    (True,False) -> (True, currentStr:resList)
    (False,True) -> (True, resList)
    (False,False) -> (False,resList)

main = do
    file <- argOrPrompt 0 "File"
    tag <- argOrPrompt 1 "Tag"
    --lineNum <- fmap read $ argOrPrompt 2 "Line number"
    contentWords <- fmap (concat . intersperse ["\n"] . tail . map words  . lines)  $ readFile file
    --print $  contentsList !! lineNum
    mapM putStr $ map spaceNn $ init $ snd $ foldr (selector $ "[" ++ tag ++ "]") (False,[]) contentWords
    
{-
    args <- getArgs
    putStrLn "Internal title: "
    inTitle <- getLine
    putStrLn "Display title: "
    dispTitle <- getLine
    putStrLn "Author(s): "
    author <- getLine
    putStrLn "dOrigin: "
    dOrigin <- getLine
    putStrLn "EXH extension:"
    ext <- getLine
    putStrLn "Comments:"
    comment <- getLine
    putStrLn "Tags (comma-separate, no spaces):"
    tags <- getLine
    let tagList = if null splitted
                  then []
                  else ["[TT]"] ++ (map (indent++) splitted) ++ ["[TT]"]
                  where splitted = splitComma tags
    let today = args !! 1
    let todayComment = if null comment
                       then ""
                       else "-[" ++ today ++ "] " ++ comment
    let contents = ["[$$]", indent ++ "$ " ++ inTitle, indent ++ "entryType bib", "[$$]", "[BB]", indent ++ "$ " ++ dispTitle, indent ++ "author " ++ author, indent ++ "dOrigin " ++ dOrigin,indent ++ "dAnnotate " ++ today, "[BB]"] ++ tagList ++ [todayComment, "[EXH]", indent ++ "- " ++ inTitle ++ "." ++ ext,"[EXH]"]
    writeFile ((args !! 0) ++ "/." ++ inTitle ++ ".gpf") $ unlines contents -}
