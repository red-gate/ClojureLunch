module Main where

import Lib
import Control.Monad.State.Strict
import System.IO
import Graphics.Gloss
import qualified Graphics.Gloss.Interface.Pure.Game as G
import Data.Monoid ((<>))

data DirectionY = Up | Down
  deriving (Show, Eq)

data Bat = Bat (Maybe DirectionY) Float
  deriving Show

type Ball = (Float, Float)

data World = MyWorld { ball :: Ball
                     , p1 :: Bat
                     , p2 :: Bat }
 deriving Show

initialBat :: Bat
initialBat = Bat Nothing 0

initialWorld = MyWorld { ball = (20,20), p1 = initialBat, p2 = initialBat}

step :: Float -> World -> World
step d (MyWorld b p1 p2) =
  MyWorld (stepBall d b)  (stepPlayer d p1) (stepPlayer d p2)

stepBall :: Float -> Ball -> Ball
stepBall delta (x,y) = (x + ballSpeed * delta, y) 

stepPlayer d b@(Bat Nothing _) = b
stepPlayer d (Bat c@(Just dir) p) = let f = case dir of
                                          Up -> (+)
                                          Down -> (-)
                                        in Bat c (f p (d * ballSpeed))   

worldToPicture :: World -> Picture
worldToPicture (MyWorld b p1 p2) = drawBall b <> (tP1 $ drawBat p1) <> (tP2 $ drawBat p2)
  where
    batTransDist = (gameWidth - batWidth) / 2
    tP1 = translate (- batTransDist) 0
    tP2 = translate batTransDist 0
    
drawBall :: Ball -> Picture  
drawBall (x,y) = translate x y (circleSolid 60)

drawBat :: Bat -> Picture
drawBat (Bat _ pos) = translate 0 pos (rectangleSolid batWidth batHeight)

toggle :: G.KeyState -> DirectionY -> Bat -> Bat
toggle G.Down dir (Bat _ p) = Bat (Just dir) p 
toggle G.Up dir (Bat (Just oldDir) p) = let d = if (dir == oldDir) then Nothing else Just oldDir in
  Bat d p 
toggle G.Up _ b = b

input :: G.Event -> World -> World
input (G.EventKey (G.Char key) state _ _) w@(MyWorld _ p1 p2)=
  case key of
    'a' -> w {p1 = toggle state Up p1}
    'z' -> w {p1 = toggle state Down p1}
    'k' -> w {p2 = toggle state Up p2}
    'm' -> w {p2 = toggle state Down p2}
    _ -> w
input _ w = w 

main :: IO ()
main = play (InWindow "Nice Window" (gameWidth, gameHeight) (10, 10)) white 30 initialWorld worldToPicture input step


    
ballSpeed :: Float
ballSpeed = 10

batWidth :: Num a => a
batWidth = 10

batHeight :: Num a => a
batHeight = 100

gameWidth :: Num a => a
gameWidth = 500

gameHeight :: Num a => a
gameHeight = gameWidth
