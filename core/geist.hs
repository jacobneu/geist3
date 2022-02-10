import System.Environment
import Control.Monad  
import Data.Char  
import Text.Read
import Nanoparsec
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

data Command = SKIP
             | CHECK
             | GET { fileName :: String, tag :: Tag }
             | SET


indent :: String
indent = "    "

doCmd :: Command -> [String] -> IO ()
doCmd SKIP _ = return ()
doCmd CHECK l =  mapM putStrLn l >> putStrLn "Done!"
doCmd (GET {fileName=fN, tag=tG}) l = do
    fileContents <- readFile fN
    let desired = runParser (getParser tG) fileContents  
    case desired of 
        [] -> return () -- putStrLn $ "[FAIL] " ++ fN ++ " has no entry for " ++ (show tG)
        (Plain res:_) -> mapM_ putStrLn $ dropWhile (=="") (lines res)
        (KVList res:_) -> mapM_ putStrLn $ map (\(key,value) -> indent ++ key ++ " " ++ value) res
        (BulletPoints res:_) -> mapM_ putStrLn [(replicate n ' ') ++ (annShow ann) ++ " " ++ rest | (n,ann,rest) <- res]
        (StList res:_) -> mapM_ (putStrLn . show) res 
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