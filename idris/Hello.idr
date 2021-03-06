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

data Expr ty = Val ty 
            | Add (Expr ty) (Expr ty) 
            | Subtraction (Expr ty) (Expr ty) 
            | Mult (Expr ty) (Expr ty)

evaluate : Neg ty => Expr ty -> ty
evaluate (Val x) = x
evaluate (Add x y) = (evaluate x)+(evaluate y)
evaluate (Subtraction x y) = (evaluate x)-(evaluate y)
evaluate (Mult x y) = (evaluate x)*(evaluate y)

Functor Expr  where
 map f (Val x) = Val $ f x
 map f (Add x y) =  Add (map f x) (map f y)
 map f (Subtraction x y) =   Subtraction (map f x) (map f y)
 map f (Mult x y) =   Mult (map f x) (map f y)

 

Show ty => Show (Expr ty) where
  show (Val x) = show x
  show (Add x y) = (show x) ++ "+" ++ (show y)
  show (Subtraction x y) = (show x) ++ "-" ++ (show y)
  show (Mult x y) = (show x) ++ "*" ++ (show y)
    
(Eq ty, Neg ty) => Eq (Expr ty) where
  (==) x y = (evaluate x) == (evaluate y)

(Ord ty, Neg ty) => Ord (Expr ty) where
  compare x y = compare (evaluate x) (evaluate y)

maxMaybe : Ord a => Maybe a -> Maybe a -> Maybe a
maxMaybe Nothing Nothing = Nothing
maxMaybe Nothing (Just x) = Just x
maxMaybe (Just x) Nothing = Just x
maxMaybe (Just x) (Just y) = Just (max x y)

(Neg ty) => Cast (Expr ty) ty where
  cast = evaluate

data Shape = Triangle Double Double
    | Rectangle Double Double
    | Circle Double


Eq Shape where
  (==) (Triangle x z) (Triangle x' z') = x == x' && z == z'
  (==) (Rectangle x z) (Rectangle x' z') = x == x' && z == z'
  (==) (Circle x) (Circle x') = x == x'
  (==) _ _ = False

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

Ord Shape where
    compare x y = compare (area x) (area y)

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

zeplWith : a -> String -> (a -> String -> Maybe (String, a)) -> IO ()
zeplWith state prompt onInput =
    do putStr prompt
       input <- getLine
       case onInput state input of
         Just (result, state') => do   
                    putStrLn result
                    zeplWith state' prompt onInput
         Nothing => pure ()

zepl : String -> (String -> String) -> IO ()
zepl prompt onInput = zeplWith () prompt (\x,y => Just $ (onInput y, ()))


readVectLen : (len : Nat) -> IO (Vect len String)
readVectLen Z = pure []
readVectLen (S k) = do x <- getLine
                       xs <- readVectLen k
                       pure (x :: xs)


readVect : IO (n ** Vect n String)
readVect = do x <- getLine
              if (x == "")
                  then pure (_ ** [])
                  else do (_ ** v) <- readVect
                          pure (_ ** (x :: v))
       

printVect : Show a => (n ** Vect n a) -> IO ()
printVect (n ** v)
    = putStrLn (show v ++ " (length " ++ show n ++ ")")

zipInputs : IO ()
zipInputs = do putStrLn "Enter first vector (blank line to end):"
               (len1 ** vec1) <- readVect
               putStrLn "Enter second vector (blank line to end):"
               (len2 ** vec2) <- readVect
               case (exactLength len2 vec1) of
                Just vec1 => putStrLn $ show $ Vect.zip vec1 vec2
                Nothing => putStrLn "Vectors are of different lengths: cannot zip"

-- checkout openFile, closeFile, fEOF, fGetLine,and writeFile

readToBlank : IO (List String)
readToBlank = do
    x <- getLine
    if x == "" 
        then pure []
        else (x  ::) <$> readToBlank 

readAndSave : IO()
readAndSave = do
    stuffToWrite <- readToBlank
    filePath <- getLine
    _ <- writeFile filePath (concat stuffToWrite)
    pure ()



    
readVectFromFile : File -> IO (n : Nat ** Vect n String)
readVectFromFile file = do
    isEnd <- fEOF file
    case not isEnd of
        True => do
            Right line <- fGetLine file | Left err =>  do
                                                        _ <- print err
                                                        pure (_** [])
            previous <- readVectFromFile file
            case previous of
                (_ ** x) => pure (_** line::x)
        False => pure (_** [])

readVectFile : (filename : String) -> IO (n ** Vect n String)
readVectFile filename = do
     Right f <- openFile filename Read | Left err => do
                                                       _ <- print err
                                                       pure (_** [])
     readVectFromFile f 

AdderType : (Nat) -> Type
AdderType Z = Int
AdderType (S k) = (Int) -> AdderType k

adder : (numargs : Nat) -> (acc : Int) -> AdderType numargs
adder Z acc = acc
adder (S k) acc = \next => adder k (next + acc)

data Format = Number Format
    | Str Format
    | Lit String Format
    | Doub Format
    | Character Format
    | End

PrintfType : Format -> Type
PrintfType End = String 
PrintfType (Lit st rest) = PrintfType rest
PrintfType (Str rest) = (String) -> PrintfType rest
PrintfType (Number rest) = (Int) -> PrintfType rest 
PrintfType (Doub rest) = (Double) -> PrintfType rest 
PrintfType (Character rest) = (Char) -> PrintfType rest 

printfFmt : (fmt : Format) -> (acc : String) -> PrintfType fmt
printfFmt (Number x) acc = \y => printfFmt x (acc ++ show y) 
printfFmt (Str x) acc = \y => printfFmt x (acc ++ y)
printfFmt (Lit x y) acc = printfFmt y (acc ++ x)
printfFmt (Doub x) acc = \y => printfFmt x (acc ++ show y)
printfFmt (Character x) acc = \y => printfFmt x (acc ++ ( pack (the (List Char) [y] )))
printfFmt End acc = acc

-- Banana %s Monkey -> (Lit "Banana " (Str (Lit " Monkey" End)))
toFormat : (xs : List Char) -> Format
toFormat [] = End
toFormat ('%' :: 's' :: xs) = Str (toFormat xs)
toFormat ('%' :: 'd' :: xs) = Number (toFormat xs)
toFormat ('%' :: 'f' :: xs) = Doub (toFormat xs)
toFormat ('%' :: 'c' :: xs) = Character (toFormat xs)
toFormat (x :: xs) = Lit ( pack (the (List Char) [x] )) (toFormat xs)

--*Hello> printfFmt (toFormat (unpack "Banana %s Monkey")) "" "eaten by"
--"'B''a''n''a''n''a'' 'eaten by' ''M''o''n''k''e''y'" : String

printf : (formatString : String) -> PrintfType (toFormat $ unpack formatString)
printf formatString = printfFmt _ ""

Matrix : (m :Nat) -> (n : Nat) -> Type
Matrix m n = Vect m (Vect n Double) 

testMatrix : Matrix 2 3
testMatrix = [[0, 0, 0],
 [0, 0, 0]]


-- TupleVect 0 ty = ()
-- TupleVect 1 ty = (ty, ())
-- TupleVect 2 ty = (ty, (ty, ()))

TupleVect : Nat -> Type -> Type
TupleVect Z _ = ()
TupleVect (S x) ty = (ty, TupleVect x ty)

test : TupleVect 4 Nat
test = (1,2,3,4,())

same_cons : { xs : List a } -> { ys : List a } ->
        xs = ys -> x :: xs = x :: ys
same_cons  = cong   

same_head : {xs : List a } -> x = y -> x::xs = y::xs
same_head {xs}  = cong {f = \x => x::xs} 

same_lists : { xs : List a } -> { ys : List a } ->
        x = y -> xs = ys -> x :: xs = y :: ys
same_lists Refl = same_cons 
    -- trans (same_cons lEq) (same_head elEq)

data ThreeEq : a -> b -> c -> Type where
    Refl : ThreeEq x x x

allSameS : (x, y, z : Nat) -> ThreeEq x y z -> ThreeEq (S x) (S y) (S z)
allSameS z z z Refl = Refl
-- do something here ... ?

{- 
myReverse : Vect n elem -> Vect n elem
myReverse [] = []
myReverse (x :: xs) = reverseProof (myReverse xs ++ [x])
    where
        reverseProof : Vect (len + 1) elem -> Vect (S len) elem
        reverseProof {len} result = rewrite plusCommutative 1 len in result
-}
myPlusCommutes : (n : Nat) -> (m : Nat) -> n + m = m + n
myPlusCommutes Z m = rewrite plusZeroRightNeutral m in Refl
myPlusCommutes (S k) m = rewrite plusSuccRightSucc k m in 
                            rewrite myPlusCommutes k (S m) in 
                                rewrite plusSuccRightSucc m k in Refl

reverseProof_nil : Vect n1 a -> Vect (plus n1 0) a
reverseProof_nil {n1} xs = rewrite plusZeroRightNeutral n1 in xs

reverseProof_xs : Vect ((S n1) + len) a -> Vect (plus n1 (S len)) a
reverseProof_xs {n1} {len} xs = rewrite sym (plusSuccRightSucc n1 len) in xs


myReverse : Vect n a -> Vect n a
myReverse xs = reverse' [] xs
    where reverse' : Vect n a -> Vect m a -> Vect (n+m) a
          reverse' acc [] = reverseProof_nil acc
          reverse' acc (x :: xs) = reverseProof_xs  (reverse' (x::acc) xs)                                


zeroNotSuc : (0 = S k) -> Void
zeroNotSuc Refl impossible
sucNotZero : (S k = 0) -> Void
sucNotZero Refl impossible

noRec : (contra : (k = j) -> Void) -> (S k = S j) -> Void
noRec contra Refl = contra Refl

checkEqNat : (num1 : Nat) -> (num2 : Nat) -> Dec (num1 = num2)
checkEqNat Z Z = Yes Refl
checkEqNat Z (S k) = No zeroNotSuc
checkEqNat (S k) Z = No sucNotZero
checkEqNat (S k) (S j) = case checkEqNat k j of
                            Yes prf => Yes (cong prf)
                            No contra => No (noRec contra)

headUnequal : DecEq a => {xs : Vect n a} -> {ys : Vect n a} -> (contra : (x = y) -> Void) -> ((x :: xs) = (y :: ys)) -> Void
headUnequal contra Refl = contra Refl

tailUnequal : DecEq a => {xs : Vect n a} -> {ys : Vect n a} -> (contra : (xs = ys) -> Void) -> ((x :: xs) = (y :: ys)) -> Void
tailUnequal contra Refl = contra Refl