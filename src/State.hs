module State (
  State(..), defaultState, 
  exit, setCode, setPwd,
  exit', setCode', setPwd',
  mkAbsolute
) where

import qualified System.Environment as Env
import qualified System.Directory   as Dir

data State = State { 
  isEnd :: Bool,
  pwd   :: String,
  home  :: String,
  eCode :: Int
} deriving Show

defaultState :: IO State
defaultState = do pwd    <- (Env.getEnv "PWD" >>= Dir.canonicalizePath)
                  home   <- Dir.getHomeDirectory
                  return $  State False pwd home 0

exit :: State -> State
exit s = s { isEnd = True }

exit' :: State -> IO State
exit' = return . exit

setCode :: State -> Int -> State
setCode s c = s { eCode = c }

setCode' :: State -> Int -> IO State
setCode' s c = return $ setCode s c

setPwd :: State -> String -> State
setPwd s p = s { pwd = cleanseEnd p }

setPwd' :: State -> String -> IO State
setPwd' s p = return $ setPwd s p

cleanseEnd :: String -> String
cleanseEnd s
 | last s == '/' = init s
 | otherwise     = s

mkAbsolute :: String -> State -> String
mkAbsolute str@('/':xs) _ = cleanseEnd str
mkAbsolute ('~':xs)     s = (cleanseEnd $ home s) ++ "/" ++ cleanseEnd xs
mkAbsolute str          s = (cleanseEnd $ pwd s)  ++ "/" ++ cleanseEnd str
