module Main

import Data.Fin
import Data.Vect

main : IO ()
main = putStrLn ?greeting


StringOrInt : Bool -> Type
StringOrInt x = case x of 
        True => Int
        False => String

getStringOrInt : (x : Bool) -> StringOrInt x
getStringOrInt z = case z of
    True =>49 * 2
    False => ?ss

someVect : Vect 3 Int
someVect = [1,2,3]

someOtherVect : Vect 6 Int
someOtherVect = someVect ++ someVect

app : (Vect d y) -> (Vect 2 y) ->  (Vect (d + 2) y)
app [] ys = ys
app (x :: xs) ys = x :: app xs ys


data Tree elem = Empty | Node (Tree elem) elem (Tree elem)

insert : Ord elem => elem -> Tree elem -> Tree elem
insert x Empty = Node Empty x Empty
insert x (Node y z w) = case compare x z of
                             LT => Node (insert x y) z w 
                             EQ => Node y z w
                             GT => Node y z (insert x w)

listToTree : Ord elem => List elem -> Tree elem 
listToTree [] = Empty
listToTree (x :: xs) = insert x (listToTree xs)

treeToList : Tree elem -> List elem
treeToList Empty = []
treeToList (Node x y z) = treeToList x ++ (y :: treeToList z)

data Expr = Val Int 
            | Add Expr Expr 
            | Subtraction Expr Expr 
            | Mult Expr Expr

evaluate : Expr -> Int
evaluate (Val x) = x
evaluate (Add x y) = (evaluate x)+(evaluate y)
evaluate (Subtraction x y) = (evaluate x)-(evaluate y)
evaluate (Mult x y) = (evaluate x)*(evaluate y)

