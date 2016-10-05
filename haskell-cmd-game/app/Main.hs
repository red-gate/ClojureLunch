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

class Direction a where
  positive :: a
  negative :: a
  
instance Direction DirectionX where
  positive = Right
  negative = Left

instance Direction DirectionY where
  positive = Up
  negative = Down
  
data Bat =
  Bat (Maybe DirectionY)
      Height
  deriving (Show)

type X = Float
type Y = Float
type Coords = (X, Y)
type Radius = Float
type Width = Float
type Height = Float
type Circle = (X, Y, Radius)
type Rectangle = (X, Y, Width, Height)
type Score = Int

data Ball =
  Ball DirectionX
       DirectionY
       Radius
       Coords
  deriving (Show)

data World = MyWorld
  { ball :: Ball
  , p1 :: Bat
  , p2 :: Bat
  , score :: (Score, Score)
  } deriving (Show)

initialBat :: Bat
initialBat = Bat Nothing 0

initialBall :: Ball
initialBall = Ball Right Down 10 (-150, 20)

initialWorld =
  MyWorld
  { ball = initialBall
  , p1 = initialBat
  , p2 = initialBat
  , score = (0,0)
  }

batRectangle :: X -> Y -> Rectangle
batRectangle x y = (x, y, batWidth, batHeight)

type Delta = Float

step :: Delta -> World -> World
step d (MyWorld b p1@(Bat _ p1y) p2@(Bat _ p2y) score) =
  let batTransDist = (gameWidth - batWidth) / 2
      rectangles =
        [batRectangle (-batTransDist) p1y, batRectangle batTransDist p2y]
      cols = edgeDetect rectangles b
  in case cols of
       (Just (GoalCollision p), _) -> scorePoint p score
       cs -> MyWorld (stepBall d cs b) (stepPlayer d p1) (stepPlayer d p2) score

data Player = P1 | P2
scorePoint :: Player -> (Score, Score) -> World
scorePoint p (one, two) =
  let newScore =
        case p of
          P1 -> (one + 1, two)
          P2 -> (one, two + 1)
  in initialWorld
     { score = newScore
     }

stepBall :: Delta -> Collisions -> Ball -> Ball
stepBall delta (xCols, yCols) (Ball dirX dirY width (x, y)) =
  let move = ballSpeed * delta
      
      dY =
        case yCols of
          Nothing -> dirY
          Just _ -> flipDirection dirY
      dX =
        case xCols of
          Nothing -> dirX
          Just _ -> flipDirection dirX
  in Ball dX dY width (updatePosition dX x move, updatePosition dY y move)


updatePosition :: (Num a,Eq b, Direction b) => b -> a -> a -> a
updatePosition dir | dir == positive = (+)
                   | dir == negative = (-)
                    
data Collision = BatCollision | WallCollision | GoalCollision Player
type Collisions = (Maybe Collision, Maybe Collision)

edgeDetect :: [Rectangle] -> Ball -> Collisions 
edgeDetect bats (Ball _ _ width (x, y)) =
  let hits = any (intersect (x, y, width)) bats
  in ( if hits
         then Just BatCollision
         else hitSide
                x
                gameWidth
                (GoalCollision
                   (if x > 0
                      then P1
                      else P2))
     , hitSide y gameHeight WallCollision)
  where
    hitSide p size side =
      if abs (p) + width >= (size / 2)
        then Just side
        else Nothing

flipDirection :: (Eq a, Direction a) => a -> a
flipDirection d | d == positive = negative
                | d == negative = positive
                | otherwise = d
  


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

drawScore :: (Score, Score) -> Picture
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

type Speed = Float

ballSpeed :: Speed
ballSpeed = 100


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
