module Command (
  Command(..),
  parseCommand, exec
) where

import           Lib
import           State
import qualified System.Environment as Env
import qualified System.Directory   as Dir
import qualified System.Process     as Proc
import           System.Exit

_c1 :: String
_c1 = "exit"

_c2 :: String
_c2 = "_printState"

_c3 :: String
_c3 = "_printEnv"

_c4 :: String
_c4 = "no"

data Command = Command {
  cmnd :: String,
  args :: [String]
} deriving Show

parseCommand :: String -> Command
parseCommand s = let sp = cmdSplit s
                  in Command (head sp) $ tail sp

getEnv :: State -> IO (State, String)
getEnv s = do env <- Env.getEnvironment
              return (s, stred env)
  where stred []         = ""
        stred ((x,y):xs) = "\t" ++ x ++ " = " ++ y ++ "\n" ++ stred xs

actRun :: State -> FilePath -> [String] -> IO (State, String)
actRun s fp args = do procHandle     <- Proc.spawnProcess fp args
                      ex             <- Proc.waitForProcess procHandle
                      let eCode      =  case ex of ExitSuccess   -> 0
                                                   ExitFailure i -> i
		      return (setCode s eCode, "")

call :: State -> Command -> IO (State, String)
call s (Command cmnd args) = do fPath <- Dir.findExecutable cmnd
                                case fPath of Nothing -> return (setCode s 127, "Unknown command " ++ cmnd)
                                              Just p  -> actRun s p args

exec :: State -> Command -> IO (State, String)
exec s c@(Command cmd args)
 | cmd == ""  = return (setCode s 0, "")
 | cmd == _c1 = return (exit s,      "")
 | cmd == _c2 = return (setCode s 0, show s)
 | cmd == _c3 = getEnv $ setCode s 0
 | cmd == _c4 = return (setCode s 0, "Please, no")
 | otherwise  = call   (setCode s 0) c
