-- file: ch04/ch04.exercises.hs
import Data.List
import Data.Char
import Prelude

safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (x:_) = Just x

safeTail :: [a] -> Maybe [a]
safeTail [] = Nothing
safeTail (_:xs) = Just xs

safeLast :: [a] -> Maybe a
safeLast [] = Nothing
safeLast (x:[]) = Just x
safeLast (x:xs) = safeLast xs

safeInit :: [a] -> Maybe [a]
safeInit [] = Nothing
safeInit [x] = Just []
safeInit (x:xs) = Just (x : unpacked)
    where
        unpacked = unpack (safeInit (xs))
        unpack (Just safe) = safe

--------------------------------------------
splitWith :: (a -> Bool) -> [a] -> [[a]]
splitWith _ [] = []
splitWith pred (x:xs)
    | pred x = splitWith pred xs

splitWith pred xs = thischunk : otherchunks
    where
        notpred = not . pred
        thischunk = takeWhile notpred xs
        otherchunks = splitWith pred (dropWhile notpred xs)

---------------------------------------------
firstWords s = unlines (map each (lines s))
    where each [] = []
          each s = head (words s)

asInt_fold :: String -> Int

asInt_fold xs
    | xs == "" = error "Empty string!"
    | xs == "-" = error "Not a integer!"
    | any (not . isDigit) xs = error (xs ++ "Not a integer!")
    | otherwise  = asInt_fold_help xs

asInt_fold_help ('-':xs) = negate (asInt_fold xs)
asInt_fold_help xs = foldl' step 0 xs
    where step y ys = (10 * y) + (digitToInt ys)
