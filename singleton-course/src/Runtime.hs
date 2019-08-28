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
mkSomeDoor ds s = case ds of 
    Opened -> fromDoor_ . (mkDoor SOpened s)
    Closed -> fromDoor_ . (mkDoor SClosed s)
    Locked -> fromDoor_ . (mkDoor SLocked s)