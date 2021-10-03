module Command (
  Command(..),
  parseCommand, exec
) where

import Lib
import State

data Command = Command {
  cmnd :: String,
  args :: [String]
} deriving Show

parseCommand :: String -> Command
parseCommand s = let sp = split sep s
                  in Command (head sp) $ tail sp

exec :: State -> Command -> IO (State, String)
exec s (Command "exit" _) = return (exit s, "")
exec s c                  = return (s,      show c)
