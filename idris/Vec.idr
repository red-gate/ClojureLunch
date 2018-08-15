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

data Elem : a -> Vect k a -> Type where
    Here : Elem x (x :: xs)
    There : (later : Elem x xs) -> Elem x (y :: xs)

Uninhabited (Elem value []) where
    uninhabited Here impossible
    uninhabited There impossible

removeElem : (value : a) -> (xs : Vect (S n) a) -> { auto prf : Elem value xs } ->
    Vect n a
removeElem value (value :: ys) { prf = Here } = ys
removeElem {n = Z} value (y :: []) { prf = There later } = absurd later
removeElem {n = (S k)} value (y :: ys){prf = There later} = y :: removeElem value ys

data ListLast : List a -> Type where
    Empty : ListLast []
    NonEmpty : (xs : List a) -> (x : a) -> ListLast (xs ++ [x])

describeHelper : (input : List Int) -> (form : ListLast input) -> String
describeHelper [] Empty = "Empty"
describeHelper (xs ++ [x]) (NonEmpty xs x) = "Non-empty, initial portion = " ++ show xs

total
listLast : (xs : List a) -> ListLast xs
listLast [] = Empty
listLast (x :: xs) = case listLast xs of
                          Empty => NonEmpty [] x
                          NonEmpty ys y => NonEmpty (x :: ys) y

--describeListEnd : List Int -> String
--describeListEnd xs = describeHelper xs (listLast xs)


describeListEnd : List Int -> String
describeListEnd input with (listLast input)
    describeListEnd [] | Empty = "Empty"
    describeListEnd (xs++[x]) | (NonEmpty xs x)
        = "Non-empty, initial portion " ++ show xs