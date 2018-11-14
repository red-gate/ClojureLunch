import Data.Primitives.Views
import System

every_other : Stream a -> Stream a
every_other (a :: b :: c) = a :: every_other c

data InfList : Type -> Type where
    (::) : (elem : v) -> Inf (InfList v) -> InfList v

implementation Functor (InfList) where
  map func (elem :: x) = func elem :: (map func x)


data Face = Heads | Tails
coinFlips : (count : Nat) -> Stream Int -> List Face
coinFlips Z stream = []
coinFlips (S k) (head :: rest) with (divides head 2)
  coinFlips (S k) (((2 * div) + 1) :: rest) | (DivBy prf) = Heads :: coinFlips k rest
  coinFlips (S k) (_ :: rest) | (DivBy prf) = Tails :: coinFlips k rest

square_root_approx : (number : Double) -> (approx : Double) -> Stream Double
square_root_approx number approx = approx :: square_root_approx number ((approx + (number / approx)) / 2)

square_root_bound : (max : Nat) -> (number : Double) -> (bound : Double) ->
    (approxs : Stream Double) -> Double

square_root_bound Z number bound (a1 :: _) = a1
square_root_bound (S n) number bound (a1 :: rest) =
    if abs(a1 * a1 - number) < bound then a1 else  square_root_bound n number bound rest

square_root : (number : Double) -> Double
square_root number = square_root_bound 100 number 0.00000000001
       (square_root_approx number number)

%default total

data InfIO : Type where
  Do : IO a -> (a -> Inf InfIO) -> InfIO


(>>=) : IO a -> (a -> Inf InfIO) -> InfIO
(>>=) = Do

data Fuel = Dry | More (Lazy Fuel)

partial
forever : Fuel
forever = More forever

loopPrint : String -> InfIO
loopPrint x = do
  putStrLn x
  loopPrint x

tank : Nat -> Fuel
tank Z = Dry
tank (S k) = More (tank k)

run : Fuel -> InfIO -> IO ()
run Dry (Do x f) = printLn "Out of fuel"
run (More x) (Do y f) = do v <- y
                           run x (f v)

quiz : Stream Int -> (score : Nat) -> InfIO
quiz (num1 :: num2 :: nums) score =
  do
    putStrLn ("Score  " ++ show score)
    putStrLn (show num1 ++ " * " ++ show num2 ++ "?")
    answer <- getLine
    if (cast answer == num1 * num2)
      then do
            putStrLn "Correct!"
            quiz nums (score + 1)
      else do
            putStrLn "Incorrect"
            quiz nums score

randoms : Int -> Stream Int
randoms seed = let seed' = 1664525 * seed + 10103904223 in
                (seed' `shiftR` 2) :: randoms seed'

arithInputs : Int -> Stream Int
arithInputs seed = map bound (randoms seed)
 where
   bound : Int -> Int
   bound x with (divides x 12)
    bound ((12 * div) + rem) | (DivBy prf) = abs rem + 1

partial
main : IO ()
main = do seed <- time
          run forever (quiz (arithInputs (fromInteger seed)) 0)

totalREPL : (prompt : String) -> (action : String -> String) -> InfIO
totalREPL prompt action = do
                             putStr prompt
                             userInput <- getLine
                             let output = action userInput
                             putStrLn output
                             totalREPL prompt action
