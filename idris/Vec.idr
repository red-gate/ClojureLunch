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

headUnequal : DecEq a => {xs : Vect n a} -> {ys : Vect n a} -> (contra : (x = y) -> Void) -> ((x :: xs) = (y :: ys)) -> Void
headUnequal contra Refl = contra Refl

tailUnequal : DecEq a => {xs : Vect n a} -> {ys : Vect n a} -> (contra : (xs = ys) -> Void) -> ((x :: xs) = (y :: ys)) -> Void
tailUnequal contra Refl = contra Refl

DecEq a => DecEq (Vect n a) where
    decEq [] [] = Yes Refl
    decEq (x::xs) (y::ys) = case decEq x y of 
        Yes Refl => case decEq xs ys of 
            Yes Refl => Yes Refl
            No contra => No $ tailUnequal contra
        No contra => No $ headUnequal contra