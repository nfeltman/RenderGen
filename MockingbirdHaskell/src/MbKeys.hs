{-# LANGUAGE DeriveFunctor #-}

module MbKeys where
import MbUtilities
import Data.Monoid
import Data.List
import Data.Ord

data KeyedList t = KeyedList { backing :: [(Int, t)]} deriving Functor

-- MERGING

pickKeyed :: Monoid t => (Int,t) -> (Int,t) -> (Int,t)
pickKeyed (k1,d1) (k2,d2) = if k1 == k2 then (k1, d1 `mappend` d2) else error "Trying to merge things with different key."

mergeByKey :: Monoid t => [KeyedList t] -> KeyedList t
mergeByKey = (KeyedList).(foldl1 (angryZipWith pickKeyed)).(map backing)

upwrap (KeyedList a) = a

mergeDisjoint :: [KeyedList t] -> KeyedList t
mergeDisjoint = (KeyedList).(sortBy $ comparing fst).concat.(map backing)


-- GRID ARRANGEMENT
                
splitMaybe n xs@(_:_) = Just (splitAt n xs) 
splitMaybe _ [] = Nothing

chunkify :: Int -> [t] -> [[t]]
chunkify n xs = unfoldr (splitMaybe n) xs

arrangeGrid :: Int -> Int -> KeyedList t -> [[t]]
arrangeGrid w h (KeyedList xs) = 
    if length xs == w*h 
    then (chunkify w).(map snd) $ sortBy (comparing fst) xs
    else error "Wrong size."