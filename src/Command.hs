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

_c5 :: String
_c5 = "cd"

data Command = Command {
  cmnd :: String,
  args :: [String]
} deriving Show

parseCommand :: String -> Command
parseCommand s = let sp = cmdSplit s
                  in Command (head sp) $ tail sp

getEnv :: State -> IO State
getEnv s = do env <- Env.getEnvironment
              _   <- putStrLn $ stred env
              return s
  where stred []         = ""
        stred ((x,y):xs) = "\t" ++ x ++ " = " ++ y ++ "\n" ++ stred xs

actRun :: State -> FilePath -> [String] -> IO State
actRun s fp args = do procHandle     <- inPwd s $ Proc.spawnProcess fp args
                      ex             <- Proc.waitForProcess procHandle
                      let eCode      =  case ex of ExitSuccess   -> 0
                                                   ExitFailure i -> i
                      setCode' s eCode

invCmd :: String -> State -> IO State
invCmd cmnd s = do _ <- putStrLn $ "Unknown command " ++ cmnd 
                   setCode' s 127

call :: State -> Command -> IO State
call s (Command cmnd args) = do fPath <- Dir.findExecutable cmnd
                                case fPath of Nothing -> invCmd cmnd s
                                              Just p  -> actRun s p args

inPwd :: State -> IO a -> IO a
inPwd s io = Dir.withCurrentDirectory (pwd s) io

cd :: State -> [String] -> IO State
cd s []     = setPwd' s $ home s
cd s (x:xs) = do let abs =  mkAbsolute x s
                 abs'    <- Dir.canonicalizePath abs
                 ex      <- Dir.doesDirectoryExist abs'
                 case ex of True  -> setPwd' s abs'
                            False -> (putStrLn $ "cd: no such file or directory: " ++ abs') >> (setCode' s 1)

exec :: State -> Command -> IO State
exec s c@(Command cmd args)
 | cmd == ""  = return $ setCode s 0
 | cmd == _c1 = return $ exit s
 | cmd == _c2 = (putStrLn $ show s) >> return (setCode s 0)
 | cmd == _c3 = getEnv $ setCode s 0
 | cmd == _c4 = (putStrLn "Please, no") >> return (setCode s 0)
 | cmd == _c5 = cd s args
 | otherwise  = call   (setCode s 0) c
