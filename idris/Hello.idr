module Main

import Data.Fin
import Data.Vect
import System

StringOrInt : Bool -> Type
StringOrInt x = case x of 
        True => Int
        False => String

getStringOrInt : (x : Bool) -> StringOrInt x
getStringOrInt z = case z of
    True =>49 * 2
    False => ?ss

-- someVect : Vect 3 Int
-- someVect = [1,2,3]

-- someOtherVect : Vect 6 Int
-- someOtherVect = someVect ++ someVect

-- app : (Vect d y) -> (Vect 2 y) ->  (Vect (d + 2) y)
-- app [] ys = ys
-- app (x :: xs) ys = x :: app xs ys


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


maxMaybe : Ord a => Maybe a -> Maybe a -> Maybe a
maxMaybe Nothing Nothing = Nothing
maxMaybe Nothing (Just x) = Just x
maxMaybe (Just x) Nothing = Just x
maxMaybe (Just x) (Just y) = Just (max x y)


data Shape = Triangle Double Double
    | Rectangle Double Double
    | Circle Double

data Picture = Primitive Shape
    | Combine Picture Picture
    | Rotate Double Picture
    | Translate Double Double Picture

rectangle : Picture
rectangle = Primitive (Rectangle 20 10)
circle : Picture
circle = Primitive (Circle 5)
triangle : Picture
triangle = Primitive (Triangle 10 10)
testPicture : Picture
testPicture = Combine (Translate 5 5 rectangle)
    (Combine (Translate 35 5 circle)
    (Translate 15 25 triangle))

area : Shape -> Double
area (Triangle base height) = 0.5 * base * height
area (Rectangle length height) = length * height
area (Circle radius) = pi * radius * radius

pictureArea : Picture -> Double
pictureArea (Primitive shape) = area shape
pictureArea (Combine pic pic1) = pictureArea pic + pictureArea pic1
pictureArea (Rotate x pic) = pictureArea pic
pictureArea (Translate x y pic) = pictureArea pic

biggestTriangle : Picture -> Maybe Double
biggestTriangle (Primitive x) = case x of
                                     (Triangle _ _) => Just $ area x
                                     (Rectangle y z) => Nothing
                                     (Circle y) => Nothing
biggestTriangle (Combine x y) = maxMaybe (biggestTriangle x) (biggestTriangle y)
biggestTriangle (Rotate x y) = biggestTriangle y
biggestTriangle (Translate x y z) = biggestTriangle z


data PowerSource = Petrol | Pedal | Electric

data Vehicle : PowerSource -> Type where
    Bicycle : Vehicle Pedal
    Car : (fuel : Nat) -> Vehicle Petrol
    Bus : (fuel : Nat) -> Vehicle Petrol
    Unicycle : Vehicle Pedal
    Motorcycle : (fuel : Nat) -> Vehicle Petrol
    ECar : (charge : Nat) -> Vehicle Electric

wheels : Vehicle power -> Nat
wheels Bicycle = 2
wheels (Car fuel) = 4
wheels (Bus fuel) = 4
wheels Unicycle = 1
wheels (Motorcycle fuel) = 2

refuel : Vehicle Petrol -> Vehicle Petrol
refuel (Car fuel) = Car 100
refuel (Bus fuel) = Bus 200
refuel (Motorcycle fuel) = Motorcycle 30


tryIndex : Integer -> Vect n a -> Maybe a
tryIndex {n} x xs = case integerToFin x n of
                         Nothing => Nothing
                         (Just y) => Just(Data.Vect.index y xs)


vectTake : (x : Fin (S n)) -> Vect n a -> Vect (cast x) a
vectTake FZ xs = []
vectTake (FS x) (y :: xs) = y :: vectTake x xs

sumEntries : Num a => (pos : Integer) -> Vect n a -> Vect n a -> Maybe a
sumEntries {n} pos xs ys = 
    case integerToFin pos n of
        Nothing => Nothing
        Just fin => Just $ index fin xs + index fin ys


printLength : IO ()
printLength = do
    x <- getLine 
    y <- pure $ Prelude.Strings.length x 
    putStrLn $ cast y  


printLonger : IO()
printLonger = getLine >>= (\first => getLine >>= (\second => 
    let lfirst = length first
        lsecond = length second in 
            case lfirst>lsecond of
                True => putStrLn first
                False => putStrLn second
    )) 

-- printLonger = do
--     first <- getLine
--     second <- getLine
--     let lfirst = Prelude.Strings.length first 
--     let lsecond = Prelude.Strings.length second
--     case lfirst>lsecond of
--         True => putStrLn first
--         False => putStrLn second
readNumber : IO (Maybe Nat)
readNumber = do
    input <- getLine
    if all isDigit (unpack input)
        then pure (Just (cast input))
        else pure Nothing
countdown : (secs : Nat) -> IO ()
countdown Z = putStrLn "Lift off!"
countdown (S secs) = do
    putStrLn (show (S secs))
    usleep 1000000
    countdown secs

countdowns : IO ()
countdowns = do 
    putStr "Enter starting number: "
    Just startNum <- readNumber | Nothing => do putStrLn "Invalid input"
    countdown startNum
    putStr "Another (y/n)? "
    yn <- getLine
    if yn == "y" 
        then countdowns
        else pure ()


guess : (target : Nat) -> IO ()
guess number = do 
    putStr "What's your guess? "
    Just g <- readNumber | Nothing => do putStrLn "Invalid input"
                                         guess number
    case compare number g of 
        LT => do putStrLn "It’s lower"
                 guess number
        GT => do putStrLn "It’s higher"
                 guess number
        _  => putStrLn "You're right!"



cguess : (target : Nat) -> (guesses : Nat) -> IO ()
cguess number guesses = do 
    putStrLn (show guesses)
    putStr "What's your guess? "
    Just g <- readNumber | Nothing => do putStrLn "Invalid input"
                                         cguess number guesses
    case compare number g of 
        LT => do putStrLn "It’s lower"
                 cguess number (S guesses)
        GT => do putStrLn "It’s higher"
                 cguess number (S guesses)
        _  => putStrLn "You're right!"


main : IO ()
main = do 
    num <- System.time
    cguess (fromIntegerNat (mod num 100)) Z

zepl : String -> (String -> String) -> IO ()

zeplWith : a -> String -> (a -> String -> Maybe (String, a)) -> IO ()