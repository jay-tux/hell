module Lib (
  prefixOf, rmPrefix, replPrefix, cmdSplit,
  sep
) where

import qualified System.IO.Strict as IOS

sep :: [Char]
sep = [' ', '\t']

empties :: [String]
empties = "" : map (\a -> [a]) sep

replPrefix :: Eq a => [a] -> [a] -> [a] -> [a]
replPrefix pref xs repl
 | prefixOf pref xs = repl ++ rmPrefix pref xs
 | otherwise        = xs

rmPrefix :: Eq a => [a] -> [a] -> [a]
rmPrefix pref xs
 | prefixOf pref xs = drop (length pref) xs
 | otherwise        = xs

prefixOf :: Eq a => [a] -> [a] -> Bool
prefixOf []     _      = True
prefixOf (x:xs) []     = False
prefixOf (x:xs) (y:ys)
 | x == y    = prefixOf xs ys
 | otherwise = False

purgeEmpty :: [String] -> [String]
purgeEmpty = filter (\a -> not $ a `elem` empties)

cmdSplit :: String -> [String]
cmdSplit s = purgeEmpty $ cmdSplit' s False False ""

-- cmdSplit' command inString escaped current
cmdSplit' :: String -> Bool -> Bool -> String -> [String]
-- base case: empty command string
cmdSplit' []     _     _     curr = [curr]
-- in string, previous escaped: append to current
cmdSplit' (x:xs) True  True  curr = cmdSplit' xs True False (curr ++ "\\" ++ [x])
cmdSplit' (x:xs) True  False curr
 | x == '"'     = cmdSplit' xs False False curr -- end of quoted string
 | x == '\\'    = cmdSplit' xs True  True  curr -- encounter \
 | otherwise    = cmdSplit' xs True  False (curr ++ [x]) -- append to quoted string
cmdSplit' (x:xs) False False curr
 | x `elem` sep = curr : cmdSplit' xs False False "" -- next word
 | x == '"'     = curr : cmdSplit' xs True  False "" -- next word, begin of quoted string
 | x == '\\'    = cmdSplit' xs False True  curr -- encounter \
 | otherwise    = cmdSplit' xs False False (curr ++ [x]) -- append to non-quoted string
-- outside of quoted string, append to current
cmdSplit' (x:xs) False True curr = cmdSplit' xs False False (curr ++ "\\" ++ [x])
