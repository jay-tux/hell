module Lib (
  prefixOf, split, 
  sep, dropEmpty
) where

import qualified System.IO.Strict as IOS

sep :: [Char]
sep = [' ', '\t']

empties :: [String]
empties = "" : map (\a -> [a]) sep

prefixOf :: Eq a => [a] -> [a] -> Bool
prefixOf []     _      = True
prefixOf (x:xs) []     = False
prefixOf (x:xs) (y:ys)
 | x == y    = prefixOf xs ys
 | otherwise = False

dropEmpty :: [String] -> [String]
dropEmpty []     = []
dropEmpty (x:xs)
 | x `elem` empties = dropEmpty xs
 | otherwise        = x : dropEmpty xs

split :: [Char] -> String -> [String]
split sep str = split' sep str ""

split' :: [Char] -> String -> String -> [String]
split' _   []      s    = [s]
split' sep (s:str) curr
 | s `elem` sep = curr : (split' sep str "")
 | otherwise    = split' sep str (curr ++ [s])
