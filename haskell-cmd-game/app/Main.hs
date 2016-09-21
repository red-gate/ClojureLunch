module Main where

import Prelude hiding (Left, Right)
import Lib
import Control.Monad.State.Strict
import System.IO
import Graphics.Gloss
import qualified Graphics.Gloss.Interface.Pure.Game as G
import Data.Monoid ((<>))

main :: IO ()
main =
  play
    (InWindow "Nice Window" (gameWidth, gameHeight) (10, 10))
    white
    30
    initialWorld
    worldToPicture
    input
    step

data DirectionX
  = Left
  | Right
  deriving (Show, Eq)

data DirectionY
  = Up
  | Down
  deriving (Show, Eq)

data Bat =
  Bat (Maybe DirectionY)
      Float
  deriving (Show)

data Ball =
  Ball DirectionX
       DirectionY
       Float
       (Float, Float)
  deriving (Show)

data World = MyWorld
  { ball :: Ball
  , p1 :: Bat
  , p2 :: Bat
  , score :: (Int, Int)
  } deriving (Show)

initialBat :: Bat
initialBat = Bat Nothing 0

initialBall :: Ball
initialBall = Ball Right Down 60 (-150, 20)

initialWorld =
  MyWorld
  { ball = initialBall
  , p1 = initialBat
  , p2 = initialBat
  , score = (0,0)
  }

batRectangle :: Float -> Float -> Rectangle
batRectangle x y = (x, y, batWidth, batHeight)

step :: Float -> World -> World
step d (MyWorld b p1@(Bat _ p1y) p2@(Bat _ p2y) score) =
  let batTransDist = (gameWidth - batWidth) / 2
  in MyWorld
       (stepBall
          d
          [batRectangle (-batTransDist) p1y, batRectangle batTransDist p2y]
          b)
       (stepPlayer d p1)
       (stepPlayer d p2)
       score

stepBall :: Float -> [Rectangle] -> Ball -> Ball
stepBall delta rectangles (Ball dirX dirY width (x, y)) =
  let move = ballSpeed * delta
      yCols = edgeDetectY x y width
      dY = case yCols of
        Nothing -> dirY
        Just _ -> flipDirectionY dirY
      xCols = edgeDetectX rectangles x y width
      dX =
        case xCols of
          Nothing -> dirX
          Just _ -> flipDirectionX dirX
  in Ball dX dY width (updatePositionX dX x move, updatePositionY dY y move)

updatePositionX :: Num a => DirectionX -> a -> a -> a
updatePositionX dirX =
  case dirX of
    Right -> (+)
    Left -> (-)

updatePositionY :: Num a => DirectionY -> a -> a -> a
updatePositionY dirY =
  case dirY of
    Up -> (+)
    Down -> (-)

edgeDetectY :: Float -> Float -> Float -> Maybe Collision
edgeDetectY x y width =
  if abs (y) + width >= (gameHeight / 2)
    then Just WallCollision
    else Nothing

flipDirectionY :: DirectionY -> DirectionY
flipDirectionY Up = Down
flipDirectionY Down = Up

data Collision = BatCollision | WallCollision 

edgeDetectX :: [Rectangle] -> Float -> Float -> Float -> Maybe Collision
edgeDetectX bats x y width =
  let hits = any (intersect (x, y, width)) bats
  in if hits
       then Just BatCollision
       else if abs (x) + width >= (gameWidth / 2)
              then Just WallCollision
              else Nothing
       

flipDirectionX :: DirectionX -> DirectionX
flipDirectionX Left = Right
flipDirectionX Right = Left

type Radius = Float

type X = Float

type Y = Float

type Width = Float

type Height = Float

type Circle = (X, Y, Radius)

type Rectangle = (X, Y, Width, Height)

intersect :: Circle -> Rectangle -> Bool
intersect (cX, cY, cR) (rX, rY, rW, rH)
  | cdx > rW / 2 + cR = False
  | cdy > rH / 2 + cR = False
  | cdx <= rW / 2 = True
  | cdy <= rH / 2 = True
  | otherwise = cornerDistance <= cR ^ 2
  where
    cdx = abs (cX - rX)
    cdy = abs (cY - rY)
    cornerDistance = (cdx - rW / 2) ^ 2 + (cdy - rH / 2) ^ 2

stepPlayer d b@(Bat Nothing _) = b
stepPlayer d (Bat c@(Just dir) p) =
  let f =
        case dir of
          Up -> (+)
          Down -> (-)
  in Bat c (f p (d * ballSpeed))

worldToPicture :: World -> Picture
worldToPicture (MyWorld b p1 p2 score) =
  drawBall b <> (tP1 $ drawBat p1) <> (tP2 $ drawBat p2) <> (drawScore score)
  where
    batTransDist = (gameWidth - batWidth) / 2
    tP1 = translate (-batTransDist) 0
    tP2 = translate batTransDist 0

drawScore :: (Int,Int) -> Picture
drawScore (i, j) =
  let f x = translate x 0 . text . show
      scoreWidth = 50
      colonWidth = 20
      w = scoreWidth + colonWidth
  in translate (colonWidth /2) 0 $ f (-w) i <> text  ":" <> f colonWidth j

drawBall :: Ball -> Picture
drawBall (Ball _ _ width (x, y)) = translate x y (circleSolid width)

drawBat :: Bat -> Picture
drawBat (Bat _ pos) = translate 0 pos (rectangleSolid batWidth batHeight)

toggle :: G.KeyState -> DirectionY -> Bat -> Bat
toggle G.Down dir (Bat _ p) = Bat (Just dir) p
toggle G.Up dir (Bat (Just oldDir) p) =
  let d =
        if (dir == oldDir)
          then Nothing
          else Just oldDir
  in Bat d p
toggle G.Up _ b = b

input :: G.Event -> World -> World
input (G.EventKey (G.Char key) state _ _) w@(MyWorld _ p1 p2 _) =
  case key of
    'a' ->
      w
      { p1 = toggle state Up p1
      }
    'z' ->
      w
      { p1 = toggle state Down p1
      }
    'k' ->
      w
      { p2 = toggle state Up p2
      }
    'm' ->
      w
      { p2 = toggle state Down p2
      }
    _ -> w
input _ w = w

ballSpeed :: Float
ballSpeed = 50


batWidth
  :: Num a
  => a
batWidth = 10

batHeight
  :: Num a
  => a
batHeight = 100

gameWidth
  :: Num a
  => a
gameWidth = 500

gameHeight
  :: Num a
  => a
gameHeight = gameWidth
