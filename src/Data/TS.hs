{-# LANGUAGE OverloadedStrings #-}



module Data.TS
(
    TS(..)
  , createTSRaw
  , writeFileTS
)
where

import Data.Time
import Data.Time.Calendar.WeekDate

-- https://hackage.haskell.org/package/vector-0.12.0.1/docs/Data-Vector.html
import qualified Data.Vector as V

type TSIndex = Int
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


