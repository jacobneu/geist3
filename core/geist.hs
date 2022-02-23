import System.Environment
import System.Directory
import System.IO
import Control.Monad  
import Data.Char  
import Text.Read
import Nanoparsec
import GTime
import GPF

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

loopCollect :: IO [String]
loopCollect = do
    newLine <- getLine
    if null newLine
    then return []
    else do 
        rest <- loopCollect
        return $ newLine:rest


data Command = SKIP
             | CHECK
             | GET { fileName :: String, tag :: Tag }
             | SET
             | LOAD { fileName :: String }
             | SNAPSHOT { fileName :: String, archiveName :: String }
             | CLEAN { fileName :: String }
             | ADD { fileName :: String, tag :: Tag }


archName :: String -> String
archName fN = let 
    repl '/' = '.'
    repl c = c
    in "archive/" ++ (map repl fN)
doCmd :: Command -> [String] -> IO ()
doCmd SKIP _ = return ()
doCmd CHECK l =  mapM putStrLn l >> putStrLn "Done!"
doCmd (GET {fileName=fN, tag=tG}) l = do
    fileContents <- readFile fN
    let desired = runParser (getExParser tG) fileContents  
    case desired of 
        [] -> return () -- putStrLn $ "[FAIL] " ++ fN ++ " has no entry for " ++ (show tG)
        (e:_) -> mapM_ putStrLn (mkStrList e)
doCmd (LOAD {fileName=fN}) l = do
    fileContents <- readFile fN
    case runParser documentParser fileContents  of
        [] -> return ()
        (d:_) -> mapM_ putStrLn (repr d)
doCmd (SNAPSHOT {fileName=fN, archiveName=arN}) l = do
    handle <- openFile fN ReadMode
    (tempName, tempHandle) <- openTempFile "." "temp"
    contents <- hGetContents handle
    hPutStr tempHandle contents
    hClose handle
    hClose tempHandle
    renameFile tempName ("archive/" ++ arN)
doCmd (CLEAN {fileName=fN}) l = do
    fileContents <- readFile fN
    case runParser documentParser fileContents  of
        [] -> error "Parse failed"
        (d:_) -> do
                    renameFile fN $ archName fN
                    writeFile fN (unlines (repr d))
doCmd (ADD {fileName=fN, tag=tG}) l = do
    fileContents <- readFile fN
    case runParser documentParser fileContents  of
        [] -> error "Parse failed"
        (d:_) -> do
            additions <- loopCollect
            let arN = archName fN
            case docAppend d tG additions of
                Nothing -> writeFile fN fileContents >> error "Failed"
                (Just d') -> writeFile arN fileContents >> writeFile fN (unlines (repr d'))
    
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
        "load" -> do
                    in_fileName <- argOrErr 1 "fileName"
                    doCmd (LOAD in_fileName) (drop 1 args)
        "snapshot" -> do 
                    in_fileName <- argOrErr 1 "fileName"
                    in_archiveName <- argOrErr 2 "archiveName"
                    doCmd (SNAPSHOT in_fileName in_archiveName) (drop 2 args)
        "clean" -> do
                    in_fileName <- argOrErr 1 "fileName"
                    doCmd (CLEAN in_fileName) (drop 1 args)
        "add" -> do
                    in_fileName <- argOrPrompt 1 "fileName"
                    in_tag <- fmap read $ argOrPrompt 2 "tag"
                    doCmd (ADD in_fileName in_tag) (drop 2 args)
        _ -> error $ "Command '" ++ cmd ++ "' not recognized."
