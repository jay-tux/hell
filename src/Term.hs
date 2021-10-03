module Term (
  prompt, runCommand
) where

import Lib
import State
import Command

prompt :: State -> String
prompt s = code (eCode s) ++ path ++ "> "
  where code 0 = ""
        code i = show i ++ " "
        path   = replPrefix (home s) (pwd s) "~"

runCommand :: String -> State -> IO (State, String)
runCommand i s = exec s $ parseCommand i
