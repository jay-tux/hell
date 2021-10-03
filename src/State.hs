module State (
  State(..),
  defaultState, exit, setCode
) where

import qualified System.Environment as Env

data State = State { 
  isEnd :: Bool,
  pwd   :: String,
  home  :: String,
  eCode :: Int
} deriving Show

defaultState :: IO State
defaultState = do pwd    <- Env.getEnv "PWD"
                  home   <- Env.getEnv "HOME"
                  return $  State False pwd home 0

exit :: State -> State
exit s = s { isEnd = True }

setCode :: State -> Int -> State
setCode s c = s { eCode = c }
