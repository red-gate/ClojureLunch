data Vect : Nat -> Type -> Type where
    Nil : Vect Z a
    (::) : (x : a) -> (xs : Vect k a) -> Vect (S k) a
%name Vect xs, ys, zs

append : Vect n elem -> Vect m elem -> Vect (n + m) elem
append [] ys = ys
append (x :: xs) ys = x :: append xs ys

zip : Vect n a -> Vect n b -> Vect n (a, b)
zip [] [] = []
zip (x :: xs) (y :: ys) = ?hole :: ?rest

(Eq ty) => Eq (Vect n ty) where
    (==) [] [] = True
    (==) (x :: xs) (y :: ys) = x == y && xs == ys

Foldable (Vect n) where 
  foldr func init [] = init
  foldr func init (x :: xs) = func x (foldr func init xs)
  --foldl func init input = ?Foldable_rhs_2
