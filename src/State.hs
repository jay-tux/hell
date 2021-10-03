module State (
  State(..),
  exit, defaultState
) where

data State = State { 
  isEnd :: Bool,
  pwd   :: String
} deriving Show

defaultState :: IO State
defaultState = return $ State False "~"

exit :: State -> State
exit (State _ pwd) = State True pwd
