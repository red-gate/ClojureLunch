module Main where

import Lib
import Control.Monad.State.Strict
import System.IO

main :: IO ()
main = do
  (runStateT loop 0)
  return ()
  
loop :: StateT Int IO ()
loop = do
  input <- liftIO getLine
  (out, exit) <- someFunc input 
  liftIO $ putStrLn out
  liftIO $ hFlush stdout
  if exit
    then return ()
    else loop
    
    
