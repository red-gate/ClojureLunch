module Main where
import Data.Text

-- https://blog.jle.im/entry/introduction-to-singletons-1.html

data Foo a = MkFoo Int

data DoorState = Opened | Closed | Locked
  deriving (Show, Eq)

data Door (s :: DoorState) = UnsafeMkDoor { doorMaterial :: String, handleShape :: String }
  deriving (Show)

  -- 'Opened etc are *type constructors* as opposed to the 
  -- data constructors Opened etc on the DoorState datatype
  --
  -- See the notes on promotion in
  -- http://dreixel.net/research/pdf/ghp.pdf
data SingDS :: DoorState -> * where
  SOpened :: SingDS 'Opened
  SClosed :: SingDS 'Closed
  SLocked :: SingDS 'Locked

  -- this works, but note that Opened here is the same
  -- *type constructor* as above ('Opened) NOT the 
  -- data constructor
  --
  -- There is discussion of the disambigutaion using the quote 
  -- https://downloads.haskell.org/~ghc/7.8.4/docs/html/users_guide/promotion.html
data SingDSx :: DoorState -> * where
    SOpenedx :: SingDSx Opened
    SClosedx :: SingDSx Closed
    SLockedx :: SingDSx Locked

closeDoor :: Door 'Opened -> Door 'Closed
closeDoor (UnsafeMkDoor m h) = UnsafeMkDoor m h

lockDoor :: Door 'Closed -> Door 'Locked
lockDoor (UnsafeMkDoor m h) = UnsafeMkDoor m h

lockAnyDoor :: SingDS s -> Door s -> Door 'Locked
lockAnyDoor = \case
    SOpened -> lockDoor . closeDoor  -- in this branch, s is 'Opened
    SClosed -> lockDoor              -- in this branch, s is 'Closed
    SLocked -> id                    -- in this branch, s is 'Locked

    -- note from the blog post ... 
    -- In Haskell, a constraint SingDSI s => is essentially the same as passing in SingDS s explicitly. 
    -- Either way, you are passing in a runtime witness that your function can use. 
    -- You can think of SingDSI s => as passing it in implicitly, and SingDS s -> as passing it in explicitly.

    -- So, it’s important to remember that lockAnyDoor and lockAnyDoor_ are the “same function”, 
    -- with the same power. They are just written in different styles – 
    -- lockAnyDoor is written in explicit style, and lockAnyDoor_ is written in implicit style.
    
lockAnyDoor_ :: SingDSI s => Door s -> Door 'Locked
lockAnyDoor_ = lockAnyDoor singDS
      
  -- let b = lockAnyDoor_ (mkDoor SOpened "a" "b")

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

doorStatus_ :: SingDSI s => Door s -> DoorState
doorStatus_ = doorStatus singDS

main :: IO ()
main = do
  let x = MkFoo 9
  let y = MkFoo 6
  let d = UnsafeMkDoor "one"  
  putStrLn "hello world"

