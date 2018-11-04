{-# LANGUAGE OverloadedStrings #-}



module Data.TS
(
    TS(..)
  , TSIndex(..)
  , createTSRaw
  , writeFileTS
  , readFileTS
  , combineTS
  , isEmptyTS
  , alignBackFillForwardTS
)
where

import Data.Utils

import Data.Time
import Data.Time.Calendar.WeekDate

import Text.CSV
import Data.Maybe
import qualified Data.Map as Map

-- https://hackage.haskell.org/package/vector-0.12.0.1/docs/Data-Vector.html
-- https://wiki.haskell.org/Numeric_Haskell:_A_Vector_Tutorial
import qualified Data.Vector as V



type TSIndex = UTCTime
data TS a = TS (V.Vector TSIndex) (V.Vector a)


createTSRaw :: (Eq a, Num a) => V.Vector TSIndex -> V.Vector a -> TS a
createTSRaw times values = TS abtimes abvalues
  where
    (abtimes, abvalues) = if (V.length times) == (V.length values) then (times, values) else (V.empty, V.empty)


instance Show a => Show (TS a) where
    show (TS times values) = concat rows
      where rows = (V.singleton "Date | Value\n") V.++ V.zipWith (\x y -> mconcat [show x," | ", show y, "\n"] ) times values


writeFileTS :: (Show a) => FilePath -> TS a -> IO ()
writeFileTS path (TS times values) =
    writeFile path tsString
      where
        -- formatTime defaultTimeLocale "%F %T (%Z)" x
        tsString = concat $ 
          (V.singleton "Date | Value\n") V.++ V.zipWith (\x y -> mconcat [show x," | ", show y, "\n"] ) times values


readFileTS :: FilePath -> IO (TS Double)
readFileTS path = do
    let tstext = readFile path
    txt <- tstext
    let ptxt = parseCSV path txt

    case ptxt of
        Left _    -> return $ createTSRaw V.empty V.empty
        Right dta -> do 
            let date_ =  either 
                    (\_-> []) 
                    (\x-> fmap (read2UTCTimeMaybe "%Y-%m-%d %H:%M:%S %Z") x) (getColumnInCSV dta "Date")
            let date = if any (== Nothing) date_ then [] else catMaybes date_                      
            let value_ = either
                    (\_-> [])
                    (\x-> fmap read2DoubleMaybe x) (getColumnInCSV dta "Value")
    
            -- Check better !!!
            let value = if any (== Nothing) value_ then [] else catMaybes value_
            return $ createTSRaw (V.fromList date) (V.fromList value)


combineTS :: (Eq a, Num a) => TS a -> TS a -> TS a
combineTS (TS t1 v1) (TS t2 v2) = TS (V.fromList $ Map.keys tv2Map) (V.fromList $ Map.elems tv2Map)
    where
        tvMap = foldl (\mm (key, value) -> Map.insert key value mm) Map.empty $ V.zip t1 v1
        tv2Map = foldl (\mm (key, value) -> Map.insert key value mm) tvMap $ V.zip t2 v2


instance (Eq a, Num a) => Semigroup (TS a) where
    (<>) = combineTS


instance (Eq a, Num a) => Monoid (TS a) where
    mempty = TS V.empty V.empty
    mappend = (<>)


isEmptyTS :: TS a -> Bool
isEmptyTS (TS index values) = (V.null index) || (V.null values)

--
-- Align and backfill time series ts to the index idx
-- ---------------------------------------------------

-- https://downloads.haskell.org/~ghc/6.12.2/docs/html/libraries/containers-0.3.0.0/Data-Map.html
alignTS' :: Num a => V.Vector UTCTime -> V.Vector (UTCTime, a) -> V.Vector (UTCTime, Maybe a)
alignTS' idx ts = if (V.null idx) || (V.null ts) then V.empty else V.zip idx allValues
     where  tvMap = foldl (\mm (key, value) -> Map.insert key value mm) Map.empty ts
            -- idx' = sort idx
            allValues = fmap (\v -> Map.lookup v tvMap) idx


backFillNothingsForward :: Num a => V.Vector (Maybe a) ->  V.Vector (Maybe a)
backFillNothingsForward vec 
    | V.null vec          = V.empty
    | V.length vec == 1   = vec
    | V.length vec == 2   = if isNothing (vec V.! 1) then V.replicate 2 (vec V.! 0) else vec
    | otherwise           = if (isJust (vec V.! 0) && isNothing (vec V.! 1)) 
                                then backFillNothingsForward $ (V.replicate 2 (vec V.! 0)) V.++ (V.drop 2 vec)
                                else  
                                    (V.singleton (vec V.! 0)) V.++ (backFillNothingsForward (V.drop 1 vec))


backFillNothingsForward2S :: (Num a, Eq a) => V.Vector (Maybe a) -> V.Vector (Maybe a)
backFillNothingsForward2S values = backfilled
      where
         backfilled_ = backFillNothingsForward values
         backfilled  = if V.any (==Nothing) backfilled_ 
                            then V.reverse $ backFillNothingsForward (V.reverse backfilled_)
                            else backfilled_


{-# INLINABLE catMaybesV #-}
catMaybesV :: V.Vector (Maybe a) -> V.Vector a
catMaybesV = V.concatMap maybeToVector


{-# INLINABLE maybeToVector #-}
maybeToVector :: Maybe a -> V.Vector a
maybeToVector Nothing = V.empty
maybeToVector (Just x) = V.singleton x


alignBackFillForwardTS :: (Eq a, Num a) => V.Vector UTCTime -> V.Vector (UTCTime, a) -> V.Vector (UTCTime, a)
alignBackFillForwardTS index ts = do
    let (tsIndex, values) = V.unzip $ alignTS' index ts
    let values' = backFillNothingsForward2S values
    if V.all (== Nothing) values'
        then V.empty
        else V.zip tsIndex ( catMaybesV values' )