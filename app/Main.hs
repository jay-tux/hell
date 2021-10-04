module Main where

import Lib
import Term
import State
import System.IO

main :: IO ()
main = loop defaultState
       where loop s' = do s          <- s'
                          putStr     $  prompt s
                          hFlush     $  stdout
                          cmnd       <- getLine
                          s''        <- runCommand cmnd s
                          case (isEnd s'') of True  -> return ()
                                              False -> loop $ return s''
