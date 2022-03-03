import System.Environment
import System.Directory
import System.IO
import Control.Monad  
import Data.Char  
import Text.Read
import Nanoparsec
import GTime
import GPF
import qualified Data.Map as Map

gDOCTREE :: String
gDOCTREE = "/home/jacobneu/doctree3/"

gARCHIVE :: String
gARCHIVE = "/home/jacobneu/archive3/"



argOrPrompt :: Int -> String -> IO String
argOrPrompt n message = do
    args <- getArgs
    if n < length args
        then return (args !! n)
        else (putStrLn (message ++ ":") >> getLine)
argOrDefault :: Int -> String -> IO String
argOrDefault n def = do
    args <- getArgs
    if n < length args
        then let res = (args !! n)
             in if res=="_" then return def else return res
        else return def


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
{-
loopCollectTS :: IO [String]
loopCollectTS = do
    newLine <- getLine
    timestamp <- getTime
    let ! actualStamp = show timestamp
    if null newLine
    then return []
    else do 
        rest <- loopCollect
        return $ ("    -[" ++ actualStamp ++ "] " ++ newLine):rest
-}
data Command = SKIP
             | CHECK
             | GET { fileName :: String, tag :: Tag }
             | SET
             | LOAD { fileName :: String }
             | SNAPSHOT { fileName :: String, archiveName :: String }
             | CLEAN { fileName :: String }
             | SAVE { fileName :: String }
             | ADD { fileName :: String, tag :: Tag }
             | STREAM { streamName :: String, date :: GDate, sD :: Bool }

wordsWhen :: (Char -> Bool) -> String -> [String]
wordsWhen p s = 
    case dropWhile p s of
        "" -> []
        s' -> w : wordsWhen p s''
            where (w, s'') = break p s'
dotSplit :: String -> [String]
dotSplit = wordsWhen (=='.')
undotSplit :: [String] -> String
undotSplit [] = ""
undotSplit l = foldl1 (\s1 s2 -> s1 ++ "." ++ s2) l

fileExt :: String -> Maybe (String,String)
fileExt s = 
    case dotSplit s of
        [] -> Nothing
        [x] -> Nothing
        l -> Just(undotSplit (init l), last l)

archName :: String -> String -> String -> String
archName sub fN post = let 
    repl '/' = '.'
    repl c = c
    cleaned = 
        case fileExt fN of
            Nothing -> (dropWhile (=='.') (map repl fN)) ++ post
            Just(name,ext) -> (dropWhile (=='.') (map repl name)) ++ "-" ++ post ++ "." ++ ext
    in gARCHIVE ++ sub ++ "/" ++ cleaned

mkStreamName :: String -> String
mkStreamName sN = let 
    repl '/' = '.'
    repl c = c
    in gDOCTREE ++ "stream/" ++ (map repl sN) ++ ".gpf"
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
    renameFile tempName (gARCHIVE ++ arN)
doCmd (CLEAN {fileName=fN}) l = do
    fileContents <- readFile fN
    today <- getDate
    archDir <- archSubDirect
    case runParser documentParser fileContents  of
        [] -> error "Parse failed"
        (d:_) -> do
                    renameFile fN $ archName archDir fN ("unclean-" ++ (eightDigit today))
                    writeFile fN (unlines (repr d))
doCmd (ADD {fileName=fN, tag=tG}) l = do
    let pre = if null l then "" else head l
    fileContents <- readFile fN
    today <- getDate
    archDir <- archSubDirect
    case runParser documentParser fileContents  of
        [] -> error "Parse failed"
        (d:_) -> do
            additions <- fmap (map (pre++)) loopCollect
            let arN = archName archDir fN (eightDigit today) 
            case docAppend d tG additions of
                Nothing -> writeFile fN fileContents >> error "Failed"
                (Just d') -> writeFile arN fileContents >> writeFile fN (unlines (repr d'))
doCmd (SAVE {fileName=fN}) l = do
    today <- getDate
    archDir <- archSubDirect
    fileContents <- readFile fN
    case runParser documentParser fileContents  of
        [] -> error "Parse failed"
        (d:_) -> writeFile (archName archDir fN (eightDigit today)) (unlines (repr d))
doCmd (STREAM {streamName=sN, date=today, sD=isStat}) l = do
    initTime <- getTime
    let adjTime = if isStat then statPlusT initTime else initTime
    let fileName = mkStreamName sN
    alreadyExists <- doesFileExist fileName
    if alreadyExists
    then doCmd (ADD fileName Main) ("    - ":l)
    else do
        stuff <- fmap (map ("    - "++)) loopCollect
        let doc = Map.fromList [
                (Meta,KVList [("$","Stream "++sN),("dOrigin",(niceFormat today))]),
                (History,BulletPoints [(4,Just (Time adjTime), "Initial write")]),
                (Main,Plain (unlines stuff))]
        writeFile fileName (unlines (repr doc))
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
        "save" -> do
                    in_fileName <- argOrErr 1 "fileName"
                    doCmd (SAVE in_fileName) (drop 1 args)
        "add" -> do
                    in_fileName <- argOrPrompt 1 "fileName"
                    in_tag <- fmap read $ argOrPrompt 2 "tag"
                    doCmd (ADD in_fileName in_tag) (drop 2 args)
        "stream" -> do
                    today <- getDate
                    sD <- fmap (\s -> s=="-s") $ argOrDefault 2 ""
                    let adjDay = if sD then statPlusD today else today
                    streamName <- argOrDefault 1 $ eightDigit adjDay
                    doCmd (STREAM streamName adjDay sD) (drop 2 args)
        _ -> error $ "Command '" ++ cmd ++ "' not recognized."
