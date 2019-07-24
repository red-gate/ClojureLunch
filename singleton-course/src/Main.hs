module Main where
import Data.Text

data Foo a = MkFoo Int

data DoorState = Opened | Closed | Locked
  deriving (Show, Eq)

data Door (s :: DoorState) = UnsafeMkDoor { doorMaterial :: String, handleShape :: String }
  deriving (Show)

data SingDS :: DoorState -> * where
  SOpened :: SingDS 'Opened
  SClosed :: SingDS 'Closed
  SLocked :: SingDS 'Locked

closeDoor :: Door 'Opened -> Door 'Closed
closeDoor (UnsafeMkDoor m h) = UnsafeMkDoor m h

lockDoor :: Door 'Closed -> Door 'Locked
lockDoor (UnsafeMkDoor m h) = UnsafeMkDoor m h

lockAnyDoor :: SingDS s -> Door s -> Door 'Locked
lockAnyDoor = \case
    SOpened -> lockDoor . closeDoor  -- in this branch, s is 'Opened
    SClosed -> lockDoor              -- in this branch, s is 'Closed
    SLocked -> id                    -- in this branch, s is 'Locked

unlockDoor :: Door 'Locked -> Door 'Closed
unlockDoor (UnsafeMkDoor m h) = UnsafeMkDoor m h

openDoor :: Door 'Closed -> Door 'Opened
openDoor (UnsafeMkDoor m h) = UnsafeMkDoor m h

fromSingDS :: SingDS s -> DoorState
fromSingDS SOpened = Opened
fromSingDS SClosed = Closed
fromSingDS SLocked = Locked
 
doorStatus :: SingDS s -> Door s -> DoorState
doorStatus s _ = fromSingDS s

mkDoor :: SingDS s -> String -> String -> Door s
mkDoor SOpened = UnsafeMkDoor
mkDoor SClosed = UnsafeMkDoor
mkDoor SLocked = UnsafeMkDoor

class SingDSI s where
  singDS :: SingDS s

instance SingDSI 'Opened where
  singDS = SOpened
instance SingDSI 'Closed where
  singDS = SClosed
instance SingDSI 'Locked where
  singDS = SLocked

lockAnyDoor_ :: SingDSI s => Door s -> Door 'Locked
lockAnyDoor_ = lockAnyDoor singDS
  
doorStatus_ :: SingDSI s => Door s -> DoorState
doorStatus_ = doorStatus singDS

main :: IO ()
main = do
  let x = MkFoo 9
  let y = MkFoo 6
  let d = UnsafeMkDoor "one"  
  putStrLn "hello world"

