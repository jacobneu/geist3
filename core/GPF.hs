module GPF where

import Control.Monad
import Control.Applicative
import Control.Arrow
import Data.Char  
import Text.Read
import Nanoparsec
import qualified Data.Map as Map

data Tag = Main | Meta | History | Info | Exhibit | Biblio | Keywords | BibTeX | ToDo deriving (Eq,Ord)
headTags :: [Tag]
headTags = [Meta,Info,Biblio,Keywords,BibTeX]
tailTags :: [Tag]
tailTags = [Exhibit,ToDo,History]
notMain :: [Tag]
notMain = headTags ++ tailTags

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
    readsPrec _ "." = [(Main,"")]
    readsPrec _ "main" = [(Main,"")]
    readsPrec _ "Main" = [(Main,"")]
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

data ContentTypeName = TagPlain | TagKVList | TagBulletPoints

getContentType :: Tag -> ContentTypeName
getContentType tag = 
    case tag of
        Meta -> TagKVList
        History -> TagBulletPoints
        Exhibit -> TagBulletPoints
        Biblio -> TagKVList
        _ -> TagPlain

data ContentType = Plain String
              | KVList [(String,String)]
              | StList [String]
              | BulletPoints [(Int,Maybe String,String)]

indent :: String
indent = "    " 


debugMode :: Bool
debugMode = False

mkStrList :: ContentType -> [String]
mkStrList c = case c of
    (Plain res) -> dropWhile (=="") (lines res)
    (KVList res) -> map (\(key,value) -> indent ++ key ++ " " ++ value) res
    (BulletPoints res) -> [(replicate n ' ') ++ "-" ++ (annShow ann) ++ " " ++ rest | (n,ann,rest) <- res]
    (StList res) -> res 

contentAppend :: ContentType -> ContentType -> ContentType
contentAppend ss (Plain []) = ss
contentAppend (Plain s) (Plain s') = 
    case (last s',s) of
        ('\n','\n':rest) -> contentAppend (Plain s') (Plain rest)
        _ -> Plain(s' ++ s)

{- KVList parser -}
arentSpaces :: String -> Bool
arentSpaces = (foldl (||) False) . (map (not . isSpace)) 

kvListParser :: Parser [(String,String)]
kvListParser = do
    content <- consume
    let contWords = map words $ filter arentSpaces $ lines content
    return [(head l, unwords $ tail l) | l <- contWords] 


{- BulletPoints parser -}
bulletParser :: Parser String -> Parser [(Int,Maybe String,String)]
bulletParser p = 
    chainl1 bulletLine (char '\n' >> return (++))
    where
        bulletLine = (do
            indentation <- countSpaces
            string "-"
            annotation <- (fmap Just $ token(delimited2 "[" "]" p)) <|> (char ' ' >> return Nothing)
            rest <- consume
            return [(indentation,annotation,rest)])
            <|> (nbspaces >> return [])

{- Helpers -}
annShow :: Maybe String -> String
annShow Nothing = ""
annShow (Just s) = "[" ++ s ++ "]"


{- DOM -}
type Dom = Map.Map Tag ContentType

mkRepr :: Dom -> Tag -> [String]
mkRepr d Main =  case (Map.lookup Main d) of
    (Just e) -> mkStrList e
    Nothing -> []
mkRepr d tag = case (Map.lookup tag d) of
    (Just e) -> (("[" ++ (show tag) ++ "]") : 
        if debugMode 
        then (map (((show tag) ++ ">>") ++) (mkStrList e))
        else (mkStrList e)) 
        ++ ["[" ++ (show tag) ++ "]"]
    Nothing -> []

repr :: Dom -> [String]
repr d = concat $ filter (not . null) $ 
    (map (mkRepr d) headTags) ++ [mkRepr d Main] ++ (map (mkRepr d) tailTags)

{- DOM parsing -}
getParser :: Tag -> Parser ContentType
getParser tag = 
    case (getContentType tag) of
        TagPlain -> fmap Plain $ delimited ("[" ++ (show tag) ++ "]") consume
        TagKVList -> fmap KVList $ delimited ("[" ++ (show tag) ++ "]") kvListParser
        TagBulletPoints -> fmap BulletPoints $ delimited ("[" ++ (show tag) ++ "]") (bulletParser consume)


docParser :: Tag -> Parser (Dom -> Dom)
docParser Main = do
    content <- consume
    return $ \startDict -> Map.insertWith contentAppend Main (Plain content) startDict
docParser tag = do
    entry <- getParser tag
    return $ \startDict -> Map.insertWith contentAppend tag entry startDict

documentParser :: Parser Dom
documentParser = do
    addContents <- chainl1 (foldl1 (<|>) (map docParser (notMain++[Main]))) (return (>>>)) 
    return $ addContents(Map.empty)

getExParser :: Tag -> Parser ContentType
getExParser tag = fmap (Map.findWithDefault (Plain "") tag) documentParser

