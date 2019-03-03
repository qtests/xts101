{-# LANGUAGE OverloadedStrings #-}


module Data.Utils
(
       read2UTCTimeMaybe
    ,  read2DoubleMaybe
    ,  isAWorkingDay
    ,  gsort
     
    -- Possibly re-write with vector !!
    ,  getColumnInCSV
    ,  delColumnInCSV
    ,  getColumnInCSVEither
    ,  removeAtIndexList
)

where

import Data.Time 
import Text.Read (readMaybe)
import Text.CSV
import Data.List (findIndex, genericIndex)
import Data.Time.Calendar.WeekDate (toWeekDate)

--sorting
import qualified Data.Vector.Algorithms.Intro as I
import qualified Data.Vector.Generic as G
import qualified Data.Vector.Unboxed as U


read2UTCTimeMaybe :: String -> String -> Maybe UTCTime
read2UTCTimeMaybe format x = parseTimeM True defaultTimeLocale format x :: Maybe UTCTime


read2DoubleMaybe :: String -> Maybe Double
read2DoubleMaybe x = readMaybe x :: Maybe Double


isAWorkingDay::UTCTime -> Bool
isAWorkingDay x =
    let myWeekDay = (toWeekDate . utctDay) x
        (_, _, aWeekDay) = myWeekDay
    in aWeekDay < 6


--- Sort vectors https://www.snoyman.com/blog/2017/12/what-makes-haskell-unique
---              https://www.schoolofhaskell.com/user/commercial/content/vector
---              https://www.schoolofhaskell.com/user/commercial/content/vector
--               https://hackage.haskell.org/package/statistics-0.15.0.0/docs/src/Statistics.Function.html

-- | Sort a vector.
sort :: U.Vector Double -> U.Vector Double
sort = G.modify I.sort
{-# NOINLINE sort #-}

-- | Sort a vector.
gsort :: (Ord e, G.Vector v e) => v e -> v e
gsort = G.modify I.sort
{-# INLINE gsort #-}

-- | Sort a vector using a custom ordering.
sortBy :: (G.Vector v e) => I.Comparison e -> v e -> v e
sortBy f = G.modify $ I.sortBy f
{-# INLINE sortBy #-}




{-| 
Below - possibly rewrite [] to vector !! 
-----------------------------------------
-}

getColumnInCSV :: CSV -> String -> Either String [String]
getColumnInCSV csv columnName =
    applyToColumnInCSV id csv columnName



getColumnInCSVEither :: Either a CSV -> String -> Either String [String]
getColumnInCSVEither csv columnName = do
      either (\_ -> Left "Error reading CSV!" )
             (\x -> applyToColumnInCSV id x columnName) csv


{-|
   Applies a function to a column (specified by a Sring) in a CSV value
   Returns (Left errorMessage) or (Right b)
-}
applyToColumnInCSV :: ([String] -> b) -> CSV -> String -> Either String b
applyToColumnInCSV func csv column =
    either
    Left
    (Right . func . elements) columnIndex
    where
        columnIndex = findColumnIndexInCSV csv column
        nfieldsInFile = length $ head csv
        records = tail $ filter (\record -> nfieldsInFile == length record) csv
        elements ci = map (\record -> genericIndex record ci) records

{-|
   Gets a column from a CSV value.
   Returns (Left errorMessage) or (Right index)
-}
findColumnIndexInCSV :: CSV -> String -> Either String Integer
findColumnIndexInCSV csv columnName =
    case lookupResponse of
          Nothing -> Left "The column does not exist in this CSV!"
          Just x  -> Right (fromIntegral x)
    where
        -- This line does the lookup to see if column is in our CSV
        lookupResponse = findIndex (== columnName) (head csv)


removeAtIndex :: Int -> [a] -> [a]
removeAtIndex i [] = []
removeAtIndex i list =
        if (i > length list || i < 0)
            then list
            else (init alist) ++ blist
        where (alist, blist) = splitAt (i + 1) list


removeAtIndexList :: [Int] -> [a] -> [a]
removeAtIndexList [] list = list
removeAtIndexList _    [] = []
removeAtIndexList (i:idx) list = removeAtIndexList idx alist
      where
        alist = removeAtIndex i list

        
delColumnInCSV :: CSV -> String -> [[Field]]
delColumnInCSV acsv columnName =
      map (removeAtIndex columnIndex') records
      where
            columnIndex = findColumnIndexInCSV acsv columnName
            columnIndex' = fromInteger $ either (\_ -> -1) id columnIndex
            nfieldsInFile = length $ head acsv
            records = tail $ filter (\record -> nfieldsInFile == length record) acsv