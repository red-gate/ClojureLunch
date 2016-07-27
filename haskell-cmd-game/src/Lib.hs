module Lib
     where

import Control.Monad.State.Strict as S
import System.Directory as D
import Control.Exception
import Data.Bifunctor (first)

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
  eDirCont <- safeContents f
  case eDirCont of
    Right dirCont ->  do
      mapM putStrLn dirCont
      return ()
    Left message -> do
      putStrLn message
      return ()


safeContents :: FilePath -> IO (Either String [FilePath])
safeContents f = do
    eith <- try $ D.getDirectoryContents f
    return $ first (\e -> show (e :: SomeException))  eith
