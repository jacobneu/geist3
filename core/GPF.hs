module GPF where

import Control.Monad
import Control.Applicative
import Data.Char  
import Text.Read
import Nanoparsec

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

-- data DatatypeName = TagPlain | TagKVList | TagBulletPoints

data ContentType = Plain String
              | KVList [(String,String)]
              | StList [String]
              | BulletPoints [(Int,Maybe String,String)]

{- KVList -}
arentSpaces :: String -> Bool
arentSpaces = (foldl (||) False) . (map (not . isSpace)) 

kvListParser :: Tag -> Parser ContentType
kvListParser tag = do
    content <- extract1 ("[" ++ (show Meta) ++ "]") consume
    let contWords = map words $ filter arentSpaces $ lines content
    return $ KVList [(head l, unwords $ tail l) | l <- contWords] 


{- BulletPoints -}
bulletParser :: Tag -> Parser String -> Parser ContentType
bulletParser tag p = 
    format $ extract1 ("[" ++ (show tag) ++ "]") $ chainl1 bulletLine (char '\n' >> return (++))
    where
        format = fmap BulletPoints 
        bulletLine = (do
            indentation <- countSpaces
            string "-"
            annotation <- (fmap Just $ token(delimited2 "[" "]" p)) <|> (char ' ' >> return Nothing)
            rest <- consume
            return [(indentation,annotation,rest)])
            <|> (nbspaces >> return [])
{-
            rest <- consume
            return (indentation,Nothing,rest)
    stuff <- extract1 ("[" ++ (show tag) ++ "]") $
        (bulletLineParser `chainl` (breakSpace >> return (++))) []
        -- <|> (consume >> return $ BulletPoints [])
    return (BulletPoints stuff)
    where
        bulletLineParser = do
            indentation <- countSpaces
            string "-"
            bulletAnnotate <- (fmap Just $ token(delimited2 "[" "]" p)) <|> (string " " >> return Nothing)
            rest <- consume
            return [(indentation,bulletAnnotate,rest)]
-}

annShow :: Maybe String -> String
annShow Nothing = ""
annShow (Just s) = "[" ++ s ++ "]"

getParser :: Tag -> Parser ContentType
getParser Meta = kvListParser Meta
getParser History = bulletParser History consume
getParser tt = fmap Plain $ extract1 ("[" ++ (show tt) ++ "]") consume


