module Main where

import Lib
import Control.Monad.State.Strict
import System.IO
import Graphics.Gloss


data World = MyWorld { timeUntilMove  :: Float, position :: (Int, Int)  }
 deriving Show

moveInterval :: Float
moveInterval = 1

initialWorld = MyWorld { timeUntilMove = moveInterval, position = (20,20) }

step :: Float -> World -> World
step delta (MyWorld timeUntilMove (x,y)) =
  if timeUntilMove - delta > 0
  then MyWorld (timeUntilMove - delta) (x,y)
  else step (delta - moveInterval) (MyWorld moveInterval (x+1, y))

worldToPicture :: World -> Picture
worldToPicture (MyWorld timeUntilMove (x,y)) =
  Translate (fromIntegral x) (fromIntegral y) (Circle 60)

main :: IO ()
main = play (InWindow "Nice Window" (200, 200) (10, 10)) white 30 initialWorld worldToPicture (\_ -> id) step


    
