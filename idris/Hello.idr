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
    | End

PrintfType : Format -> Type
PrintfType End = String 
PrintfType (Lit st rest) = PrintfType rest
PrintfType (Str rest) = (String) -> PrintfType rest
PrintfType (Number rest) = (Int) -> PrintfType rest 

printfFmt : (fmt : Format) -> (acc : String) -> PrintfType fmt
printfFmt (Number x) acc = \y => printfFmt x (acc ++ show y) 
printfFmt (Str x) acc = \y => printfFmt x (acc ++ y)
printfFmt (Lit x y) acc = printfFmt y (acc ++ x)
printfFmt End acc = acc

-- Banana %s Monkey -> (Lit "Banana " (Str (Lit " Monkey" End)))
toFormat : (xs : List Char) -> Format
toFormat [] = End
toFormat ('%' :: 's' :: xs) = Str (toFormat xs)
toFormat ('%' :: 'd' :: xs) = Number (toFormat xs)
toFormat (x :: xs) = Lit ( x) (toFormat xs)

--*Hello> printfFmt (toFormat (unpack "Banana %s Monkey")) "" "eaten by"
--"'B''a''n''a''n''a'' 'eaten by' ''M''o''n''k''e''y'" : String

printf : (formatString : String) -> PrintfType (toFormat $ unpack formatString)
printf formatString = printfFmt (toFormat $ unpack formatString) ""
