import Data.Primitives.Views

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


