module GTime where

import Data.Time.Clock
import Data.Time.Calendar
import Data.Time.LocalTime
import Nanoparsec
import Text.Printf
import Control.Applicative
import Control.Monad

leapYear :: Int -> Bool
leapYear yy = (yy `mod` 400 == 0) || ((yy `mod` 4 == 0) && (yy `mod` 100 /= 0))

daysOf :: Int -> Int -> Int
daysOf _ 1 = 31
daysOf _ 3 = 31
daysOf _ 5 = 31
daysOf _ 7 = 31
daysOf _ 8 = 31
daysOf _ 10 = 31
daysOf _ 12 = 31
daysOf _ 4 = 30
daysOf _ 6 = 30
daysOf _ 9 = 30
daysOf _ 11 = 30
daysOf yy 2 = if (leapYear yy) then 29 else 28

data GDate = GDate {year :: Int, month :: Int, day :: Int}

yearParser :: Parser Int
yearParser = fmap read (nMany numerical 4)
{-
    string "20"
    decade <- numerical
    yy <- numerical
    return $ 2000 + (read [decade,yy])
-}
monthParser :: Parser Int
monthParser = 
    (do
        m <- fmap read $ nMany numerical 2
        if (0<m && m<=12) then return m else empty
    )
dayParser :: Int -> Parser Int
dayParser max = 
    (do
        d <- fmap read $ nMany numerical 2
        if (0<d && d<=max) then return d else empty
    )

dSeparator :: Parser String
dSeparator = (string " ")<|>(string "-")

eightDigit :: GDate -> String
eightDigit (GDate {year=yy, month=mm, day=dd}) =
    (printf "%04d" yy)++(printf "%02d" mm)++(printf "%02d" dd)

monthName :: Int -> String
monthName n = case n of
    1 -> "January"
    2 -> "February"
    3 -> "March"
    4 -> "April"
    5 -> "May"
    6 -> "June"
    7 -> "July"
    8 -> "August"
    9 -> "September"
    10 -> "October"
    11 -> "November"
    12 -> "December"

niceFormat :: GDate -> String
niceFormat (GDate {year=yy, month=mm, day=dd}) = (printf "%02d" dd)++" "++(monthName mm)++" "++(printf "%04d" yy)

dateParser :: Parser GDate
dateParser = 
-- 8-digit format
    (do
        yy <- yearParser
        perhaps dSeparator        
        mm <- monthParser
        perhaps dSeparator
        dd <- dayParser (daysOf yy mm)
        return $ GDate yy mm dd
    )
        

data GTime = GTime {hour :: Int, minute :: Int}

instance Show GTime where
    show (GTime {hour=hh, minute=mm}) = (show hh)++":"++(show mm)

tSeparator :: Parser String
tSeparator = (string " ")<|>(string ":")

hourParser :: Parser Int
hourParser =
    (do
        h <- fmap read $ nMany numerical 2
        if (-6<h && h<=30) then return h else empty
    )

minuteParser :: Parser Int 
minuteParser =
    (do
        m <- fmap read $ nMany numerical 2
        if (0<=m && m<60) then return m else empty
    )

timeParser :: Parser GTime
timeParser = 
    (do
        hh <- hourParser
        perhaps tSeparator        
        mm <- minuteParser
        return $ GTime hh mm
    )
data GDateTime = GDateTime {year' :: Int, month' :: Int, day' :: Int, hour' :: Int, minute' :: Int}




getDate :: IO GDate
getDate = do
    t <- getCurrentTime
    let (yy,mm,dd) = (toGregorian . utctDay) t
    return $ GDate (fromIntegral yy) mm dd

getTime :: IO GTime
getTime = do
    t <- getCurrentTime
    z <- getCurrentTimeZone
    let (TimeOfDay hour minute _) = localTimeOfDay $ utcToLocalTime z t
    return $ GTime hour minute

archSubDirect :: IO String
archSubDirect = do
    GDate yy mm _ <- getDate
    return $ (printf "%04d" yy) ++ "-" ++ (printf "%02d" mm)
{-
main = do
    (year,month,day) <- getDate
    (hour,minute) <- getTime
    putStrLn $ show (year,month,day,hour,minute)
-}


statPlusT :: GTime -> GTime
statPlusT (GTime  hh mm) = GTime (hh+24) mm 
statPlusD :: GDate -> GDate
statPlusD (GDate yy mm dd) = GDate yy mm (dd-1)
