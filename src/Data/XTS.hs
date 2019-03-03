{-# LANGUAGE OverloadedStrings #-}

-- TODO
--- 1. Sort vectors https://www.snoyman.com/blog/2017/12/what-makes-haskell-unique

module Data.XTS
(
      XTS(..)
    , createXTSRaw
    , readFileXTS
    , writeFileXTS
    , convertTS2XTS
    , isEmptyXTS
    , combineXTSnTS
)
where

-- https://hackage.haskell.org/package/vector-0.12.0.1/docs/Data-Vector.html
-- https://wiki.haskell.org/Numeric_Haskell:_A_Vector_Tutorial
import qualified Data.Vector as V
-- import Data.Vector.Algorithms.Merge (sort)

import Data.TS
import Data.Utils

import Text.CSV

import Data.Time
import Data.Maybe (catMaybes, isNothing, isJust)
import Data.List (transpose)
import qualified Data.Map as Map


type ColXTS a = V.Vector a
type ColNameXTS = String
data XTS a = XTS (V.Vector TSIndex) (V.Vector (ColXTS a)) (V.Vector ColNameXTS)


createXTSRaw :: (Eq a, Num a) => V.Vector TSIndex -> V.Vector (ColXTS a) -> V.Vector ColNameXTS -> XTS a
createXTSRaw times values colnames = XTS abtimes abvalues colnames
    where
        (abtimes, abvalues) = if (V.length times) == (V.length $ values V.! 0) then (times, values) else (V.empty, V.empty)


readFileXTS :: FilePath -> IO (XTS Double)
readFileXTS path = do
    let tstext = readFile path
    txt <- tstext
    let ptxt = parseCSV path txt
    case ptxt of
        Left _    -> return $ createXTSRaw V.empty V.empty V.empty
        Right dta -> do
            let dates_ = either
                         (\_ -> [])
                         (\x -> fmap (read2UTCTimeMaybe "%Y-%m-%d %H:%M:%S %Z") x ) $ getColumnInCSV dta "Date"

            let dates = if any (== Nothing) dates_ then [] else catMaybes dates_
            let restD_ = (fmap . fmap ) read2DoubleMaybe $ transpose $ delColumnInCSV dta "Date"

            -- Check better !!!
            let restD = if any (== Nothing) (concat restD_) then [] else fmap catMaybes $ restD_ 
            let colnames = if (length dta == 0) then [] else filter (/= "Date") $ head dta
            return $ createXTSRaw (V.fromList dates) (V.fromList $ map V.fromList restD) (V.fromList colnames)


preparePrinting :: V.Vector String -> String -> String
preparePrinting dta sep = foldl (\x y -> x ++ sep ++ y ) "" dta


-- https://stackoverflow.com/questions/2578930/understanding-this-matrix-transposition-function-in-haskell
-- https://exercism.io/tracks/haskell/exercises/matrix/solutions/f2a0b3fb86494272a2ee3f2a
-- Should not have any empty vectors inside the vector
transposeVecOfVec :: V.Vector (V.Vector a) -> V.Vector (V.Vector a)
transposeVecOfVec m = if (V.null m || (any V.null m) )
                        then V.empty 
                        else (V.singleton $ V.map V.head m) V.++ transposeVecOfVec (V.map V.tail m) 


writeFileXTS :: (Show a) => FilePath -> XTS a -> IO ()
writeFileXTS path (XTS times values colNames) = writeFile path tsString
    where
        -- formatTime defaultTimeLocale "%F %T (%Z)" x
        tsString = concat $ 
            (V.singleton $ mconcat ["Date" ++ (preparePrinting colNames ",") ++ "\n"] ) V.++ 
            V.zipWith 
                (\x y -> mconcat [show x, preparePrinting y ",", "\n"] ) 
                times ((V.map . V.map) show $ transposeVecOfVec values)


instance Show a => Show (XTS a) where
    show (XTS times values colNames) = concat rows
        where rows = (V.singleton $ mconcat ["Date" ++ (preparePrinting colNames ",") ++ "\n"] ) V.++ 
                    V.zipWith (\x y -> mconcat [show x, preparePrinting y " | ", "\n"] )
                        times ((V.map . V.map) show $ transposeVecOfVec values)


convertTS2XTS :: String -> TS a -> XTS a
convertTS2XTS colName (TS index value) = XTS index (V.singleton value) (V.singleton colName)


isEmptyXTS :: XTS a -> Bool
isEmptyXTS (XTS index values colNames) = (V.null index) || (V.null values) || (V.null colNames)


combineXTSnTS :: (Eq a, Num a) => XTS a -> String -> TS a -> XTS a
combineXTSnTS (XTS xindex xdata xcolNames) colName (TS index value) 
    | isEmptyXTS (XTS xindex xdata xcolNames) = convertTS2XTS colName (TS index value)
    | isEmptyTS  (TS index value)             = XTS xindex xdata xcolNames
    | otherwise                               = fts       

    where
        ats = alignBackFillForwardTS xindex (V.zip index value)
        fts = if V.null ats 
                    then XTS xindex xdata xcolNames
                    else XTS xindex (xdata V.++ (V.singleton $ snd $ V.unzip ats )) (xcolNames V.++ (V.singleton colName))


-- To be done ...
combineXTSnXTS :: (Eq a, Num a) => XTS a -> XTS a -> XTS a
combineXTSnXTS  (XTS xindex xdata xcolNames) (XTS yindex ydata ycolNames) = undefined


instance (Eq a, Num a) => Semigroup (XTS a) where
    (<>) = combineXTSnXTS


instance (Eq a, Num a) => Monoid (XTS a) where
    mempty = XTS V.empty V.empty V.empty
    mappend = (<>)