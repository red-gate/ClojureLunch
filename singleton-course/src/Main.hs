module Main where
import Data.Text
import Data.Singletons
import Data.Singletons.TH

-- https://blog.jle.im/entry/introduction-to-singletons-1.html

data Foo a = MkFoo Int

data DoorState = Opened | Closed | Locked
  deriving (Show, Eq)

genSingletons [''DoorState]

data Door (s :: DoorState) = UnsafeMkDoor { doorMaterial :: String, handleShape :: String }
  deriving (Show)

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

lockAnyDoor :: Sing s -> Door s -> Door 'Locked
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
    
lockAnyDoor_ :: SingI s => Door s -> Door 'Locked
lockAnyDoor_ = lockAnyDoor sing
      
  -- let b = lockAnyDoor_ (mkDoor SOpened "a" "b")

unlockDoor :: Door 'Locked -> Door 'Closed
unlockDoor (UnsafeMkDoor m h) = UnsafeMkDoor m h

openDoor :: Door 'Closed -> Door 'Opened
openDoor (UnsafeMkDoor m h) = UnsafeMkDoor m h

  -- The door is in a strange initial state
  --   let d = UnsafeMkDoor "iron" "circular"
  
  -- It's in some kind of quantum state (so you can open or close it)

  -- Prelude Main> :t d
  -- d :: Door s

  -- However, you can force the initial state 

  -- Prelude Main> let d = UnsafeMkDoor "iron" "circular" :: Door Opened
  -- Prelude Main> :t d
  -- d :: Door 'Opened

doorStatus :: Sing s -> Door s -> DoorState
doorStatus s _ = fromSing s

mkDoor :: Sing s -> String ->  String -> Door s
mkDoor _ = UnsafeMkDoor

doorStatus_ :: SingI s => Door s -> DoorState
doorStatus_ = doorStatus sing

main :: IO ()
main = do
  let x = MkFoo 9
  let y = MkFoo 6
  let d = UnsafeMkDoor "iron" "circular"
  putStrLn "hello world"