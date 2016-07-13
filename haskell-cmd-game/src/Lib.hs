module Lib
     where

import Control.Monad.State.Strict as S
import System.Directory as D

manyFunc :: ((String, Bool), Int)
manyFunc = S.runState (do
                          stat
                          stat)
           0

stat :: State Int (String, Bool)
stat = do
  S.modify (+ 1)
  return ("Hello", True) 



someFunc :: String -> StateT Int IO (String, Bool)
someFunc s = do
  S.modify (+ 1)
  i <- S.get
  liftIO $ printContents s
  return (s ++ " " ++ (show i) ++ " : " ++ s, s == "Exit")


printContents :: FilePath -> IO ()
printContents f = do
  dirCont <- D.getDirectoryContents f
  mapM putStrLn dirCont
  return ()
