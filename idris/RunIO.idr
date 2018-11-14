%default total
data RunIO : Type -> Type where
  Quit : a -> RunIO a
  Do : IO a -> (a -> Inf (RunIO b)) -> RunIO b

(>>=) : IO a -> (a -> Inf (RunIO b)) -> RunIO b
(>>=) = Do

greet : RunIO ()
greet = do
  putStr "Enter your name: "
  name <- getLine
  if name == ""
    then do
      putStrLn "Bye bye!"
      Quit ()
    else do
      putStrLn ("Hello " ++ name)
      greet


data Fuel = Dry | More (Lazy Fuel)

partial
forever : Fuel
forever = More forever


tank : Nat -> Fuel
tank Z = Dry
tank (S k) = More (tank k)

run : Fuel -> RunIO a -> IO (Maybe a)
run _ (Quit x) = pure $ Just x
run Dry (Do x f) = pure Nothing
run (More x) (Do y f) = do v <- y
                           run x (f v)
