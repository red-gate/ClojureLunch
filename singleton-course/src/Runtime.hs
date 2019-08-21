module Main where
    import Data.Text
    import Data.Singletons
    import Data.Singletons.TH
    
    -- https://blog.jle.im/entry/introduction-to-singletons-2.html
    
   
    data DoorState = Opened | Closed | Locked
     deriving (Show, Eq)

    genSingletons [''DoorState]

    data SomeDoor :: Type where
        MkSomeDoor ::
          { someDoorState    :: DoorState
          , someDoorMaterial :: String
          } -> SomeDoor
    
    main :: IO ()
    main = do
      putStrLn "hello world"