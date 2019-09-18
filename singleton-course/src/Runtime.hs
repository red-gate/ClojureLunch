module Runtime where
import Data.Text
import Data.Singletons
import Data.Singletons.TH
    
import Main

-- https://blog.jle.im/entry/introduction-to-singletons-2.html

-- stack ghc   -- ./src/Runtime.hs -ddump-splices -XTemplateHaskell

-- getting these from Main now
-- data DoorState = Opened | Closed | Locked
--   deriving (Show, Eq)

-- genSingletons [''DoorState]

-- data Door :: DoorState -> * where
--   UnsafeMkDoor :: { doorMaterial :: String } -> Door s

data SomeDoor :: * where
  MkSomeDoor :: Sing s -> Door s -> SomeDoor

instance Show SomeDoor where
  show (MkSomeDoor s d) = show d

fromDoor :: Sing s -> Door s -> SomeDoor
fromDoor = MkSomeDoor

fromDoor_ :: SingI s => Door s -> SomeDoor
fromDoor_ = fromDoor sing

closeSomeOpenedDoor :: SomeDoor -> Maybe SomeDoor
closeSomeOpenedDoor (MkSomeDoor s d) = case s of
    SOpened -> Just . fromDoor_ $ closeDoor d
    SClosed -> Nothing
    SLocked -> Nothing

lockAnySomeDoor :: SomeDoor -> SomeDoor
lockAnySomeDoor (MkSomeDoor s d) = fromDoor_ $ lockAnyDoor s d

mkSomeDoor :: DoorState -> String -> String -> SomeDoor
mkSomeDoor ds st = case toSing ds of
  SomeSing s -> fromDoor s . mkDoor s st 

withDoor :: DoorState -> String -> (forall s. Sing s -> Door s -> r) -> r
withDoor ds x f = case ds of
  Opened -> f SOpened (mkDoor SOpened x "d")
  Closed -> f SClosed (mkDoor SClosed x "d")
  Locked -> f SLocked (mkDoor SLocked x "d")

data OldSomeDoor :: * where
  OldMkSomeDoor :: DoorState -> String -> OldSomeDoor

toOld :: SomeDoor -> OldSomeDoor
toOld (MkSomeDoor sing (UnsafeMkDoor colour _))= OldMkSomeDoor (fromSing sing) colour

fromOld :: OldSomeDoor -> SomeDoor
fromOld (OldMkSomeDoor ds color)= withSomeSing  ds (\singDoorState -> MkSomeDoor singDoorState (mkDoor singDoorState color "random"))

-- unlockDoor :: Int -> Door 'Locked -> Maybe (Door 'Closed)

unlockSomeDoor :: Int -> Door 'Locked -> SomeDoor
unlockSomeDoor password door = case newUnlockDoor password door of
  Nothing -> fromDoor_ door
  Just newDoor -> fromDoor_ newDoor

-- openAnyDoor :: SingI s => Int -> Door s -> Maybe (Door 'Opened)

openAnySomeDoor :: Int -> SomeDoor -> SomeDoor
openAnySomeDoor password (MkSomeDoor sing door) = withSingI sing (case openAnyDoor password door of
  Nothing -> fromDoor_ door
  Just newDoor -> fromDoor_ newDoor)

