module Nanoparsec where

import Data.Char
import Data.List
import Control.Monad
import Control.Applicative


newtype Parser a = Parser { parse :: String -> [(a,String)] }

selectCorrect :: [(a,String)] -> a
selectCorrect [] = error "Parse failed"
selectCorrect l = 
    case filter (null . snd) l of
        [] -> error "Parser didn't consume whole string"
        ((a,_):_) -> a

runParser :: Parser a -> String -> [a]
runParser m s =  map fst $ parse m s

item :: Parser Char
item = Parser $ \s ->
    case s of
     [] -> []
     (c:cs) -> [(c,cs)]

instance Functor Parser where
    fmap f (Parser cs) = Parser $ \s -> [(f a,b) | (a,b) <- cs s]
instance Applicative Parser where
    pure = return
    (Parser cs1) <*> (Parser cs2) = Parser $ \s -> [(f a, s2) | (f,s1) <- cs1 s, (a,s2) <- cs2 s1]
instance Monad Parser where
    return a = Parser (\s -> [(a,s)])
    p >>= f = Parser $ \s -> concatMap (\(a,s') -> parse (f a) s') $ parse p s
instance Alternative Parser where
    empty = Parser (\s -> [])
    p <|> q = Parser $ \s -> 
        case parse p s of
            [] -> parse q s
            res -> res
instance MonadPlus Parser where
    mzero = empty
    mplus p q = Parser $ \s -> parse p s ++ parse q s

andThen :: Parser a -> Parser a -> (a -> a -> a) -> Parser a
(p `andThen` q) cmb = do
    m <- p
    n <- q
    return (cmb m n)

satisfy :: (Char -> Bool) -> Parser Char
satisfy p = do
    c <- item
    if p c
    then return c
    else empty

anyChar :: Parser Char
anyChar = satisfy (\_ -> True)


suffixes :: [a] -> [[a]]
suffixes [] = [[]]
suffixes (c:cs) = (c:cs):(suffixes cs)

oneOf :: [Char] -> Parser Char
oneOf s = satisfy (flip elem s)

chainl1 :: Parser a -> Parser (a -> a -> a) -> Parser a
p `chainl1` op = do {a <- p; rest a}
    where rest a = (do f <- op
                       b <- p
                       rest (f a b))
                   <|> return a

chainl :: Parser a -> Parser (a -> a -> a) -> a -> Parser a
chainl p op a = (p `chainl1` op) <|> return a

char :: Char -> Parser Char
char c = satisfy (c ==)

spaces :: Parser String
spaces = many $ oneOf " \n\t\r"

nbspaces :: Parser String
nbspaces = many $ oneOf " \t"
alpha :: Parser Char
alpha = oneOf "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"
numerical :: Parser Char
numerical = oneOf "0123456789"
symbol :: Parser Char
symbol = oneOf "`~!@#$%^&*-_=+[{]};:'\",<.>/?"
alphanum :: Parser String
alphanum = many (alpha <|> numerical)
letterWord :: Parser String
letterWord = many alpha
validWord :: Parser String
validWord = many (alpha <|> numerical <|> symbol)
lineString :: Parser String
lineString = validWord `chainl1` (nbspaces >> return (\ s1 s2 -> s1 ++ " " ++ s2))

token :: Parser a -> Parser a
token p = do { a <- p; spaces; return a }
pre :: Parser a -> Parser a
pre p = spaces >> p

countSpaces :: Parser Int
countSpaces = do
    indentation <- nbspaces
    return (length indentation)

breakSpace :: Parser String
breakSpace = many $ pre(oneOf "\n\r")

string :: String -> Parser String
string "" = return ""
string (c:cs) = do { char c; string cs; return (c:cs) }

literal :: String -> Parser String
literal s = token(string s)

delimited :: String -> Parser a -> Parser a
delimited s m = do
    string s
    n <- m
    string s
    return n

delimited2 :: String -> String -> Parser a -> Parser a
delimited2 s1 s2 m = do
    string s1
    n <- m
    string s2
    return n

parens :: Parser a -> Parser a
parens = delimited2 "(" ")"

splits :: [a] -> [([a],[a])]
splits [] = [([],[])]
splits (c:cs) = ([],c:cs) : [(c:hd,tl) | (hd,tl) <- splits cs]

consume :: Parser String
consume = Parser splits

ignoreUntilDelim :: String -> Parser a -> Parser a
ignoreUntilDelim tag p = do
    consume
    delimited tag p

extract1 :: String -> Parser a -> Parser a
extract1 tag p = do
    n <- ignoreUntilDelim tag p
    consume
    return n

extractMany :: String -> (a -> a -> a) -> Parser a -> Parser a
extractMany tag cmb p = do {a <- ignoreUntilDelim tag p; rest a}
    where
        rest a = (do 
                    a' <- extractMany tag cmb p
                    return (cmb a a'))
                 <|> (consume >> return a)
    


test :: Parser [String]
test = extractMany "[HH]" (++) (fmap (:[]) consume)

{-
main = forever $ do
    putStr "> "
    a <- getLine
    mapM print $ runParser test a -}
