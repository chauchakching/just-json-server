module MyUtil where

import RIO (fromMaybe)
import RIO.List (findIndex)
import Debug.Trace (trace)

replaceOrInsertElem :: a -> (a -> Bool) -> [a] -> [a]
replaceOrInsertElem x f xs = firstHalf ++ [x] ++ drop 1 secondHalf
  where
    (firstHalf, secondHalf) = splitAt i xs
    i = fromMaybe (length xs) maybeIndex
    maybeIndex = findIndex f xs

debug :: Show a => String -> a -> a
debug str a = trace (str ++ ": " ++ show a) a