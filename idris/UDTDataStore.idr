module Main
import Data.Vect
import Data.Fin

infixr 5 .+.

data Schema = SString 
             | SInt
             | (.+.) Schema Schema

SchemaType : Schema -> Type
SchemaType SString = String
SchemaType SInt = Int
SchemaType (x .+. y) = (SchemaType x, SchemaType y)

data DataStore : Type where
    MkData : (schema : Schema) -> (size : Nat) -> 
    (items : Vect size (SchemaType schema)) -> DataStore

data Token : Type where  
    LBRA : Nat -> Token
    RBRA : Nat -> Token
    INT : Int -> Token
    COMMA : Nat -> Token
    CHAR : Char -> Token

implementation Eq Token where
    (==) (LBRA k) (LBRA j)  = k == j
    (==) (RBRA k) (RBRA j) = k == j
    (==) (INT x) (INT y) = x == y
    (==) (COMMA k) (COMMA j) = k == j
    (==) (CHAR x) (CHAR y) = x == y
    (==) _ _ = False


Tokenize : List Char -> Nat -> List Token
Tokenize (',' :: rest) depth = COMMA depth :: Tokenize rest depth
Tokenize ('(' :: rest) depth = LBRA (S depth) :: Tokenize rest (S depth)
Tokenize (')' :: rest) (S depth) = RBRA (S depth) :: Tokenize rest depth
Tokenize (x :: rest) depth = if isDigit x
                       then INT (cast $ the String (cast x)) :: Tokenize rest depth
                       else CHAR x :: Tokenize rest depth
Tokenize _ _ = []

tokenize : List Char -> List Token
tokenize x = Tokenize x 0 


schema : DataStore -> Schema
schema (MkData schema' size' items') = schema'
size : DataStore -> Nat
size (MkData schema' size' items') = size'

items : (store : DataStore) -> Vect (size store) (SchemaType (schema store))
items (MkData _ size' items') = items'

addToStore : (store : DataStore) -> (SchemaType (schema store)) -> DataStore
addToStore (MkData s size items') newitem = MkData _ _ (addToData items')
 where
    addToData : Vect old (SchemaType s)  -> Vect (S old) (SchemaType s) 
    addToData [] = [newitem]
    addToData (item :: items) = item :: addToData items

stringInItem : (schema: Schema) -> String -> (SchemaType schema) -> Bool
stringInItem SString x y = Strings.isInfixOf x y
stringInItem SInt x y = False
stringInItem (z1 .+. z2) x (y1, y2) = (stringInItem z1 x y1) || (stringInItem z2 x y2) 

search : (store: DataStore) -> String -> (p ** Vect p (Nat, SchemaType (schema store)))
search store subs =  
    let positions = map cast $ range { len = size store}  in
    Vect.filter (stringInItem (schema store) subs . snd) (Vect.zip positions (items store))

stringify : (schema: Schema) -> (SchemaType schema) -> String
stringify SInt x = show x
stringify SString x = x
stringify (z1 .+. z2) (x,y) = "(" ++ stringify z1 x++ "," ++ stringify z2 y ++ ")"

searchResultToString : (store: DataStore) -> (p ** Vect p (Nat, SchemaType (schema store))) -> String
searchResultToString store (_ ** ((p,v) :: xs)) = show p ++ " -> " ++ (stringify (schema store) v) ++ "\n" ++ (searchResultToString store(_ ** xs))
searchResultToString _ _ = ""

data Command = 
    Add String
    | Get Integer
    | Size
    | Search String
    | Quit

parseCommand : (cmd : String) -> (args : String) ->
    Maybe Command
parseCommand "add" str = Just (Add str)
parseCommand "get" val = case all isDigit (unpack val) of
    False => Nothing
    True => Just (Get (cast val))
parseCommand "size" "" = Just (Size)
parseCommand "quit" "" = Just Quit
parseCommand "search" str = Just (Search str)
parseCommand _ _ = Nothing

parse : (input : String) -> Maybe Command
parse input = case span (/= ' ') input of 
    (cmd, arg) => parseCommand cmd (ltrim arg)

tryIndex : Integer -> Vect n a -> Maybe a
tryIndex {n} x xs = case integerToFin x n of
                        Nothing => Nothing
                        (Just y) => Just(Data.Vect.index y xs)

--(2,(3,4))
--011122210
parseValue' : (schema: Schema) -> (a: List Token) -> Maybe(SchemaType schema)
parseValue' SInt input = case takeWhile isDig input of
        (digits) => let dig = (\(INT x) => x) <$> digits 
                        in Just (undigit dig)
    where 
          isDig : Token -> Bool
          isDig (INT _) = True
          isDig _ = False
          undigit : List Int -> Int
          undigit xs = (cast $ the String $ concat (cast <$> xs))
parseValue' (p1 .+. p2) ((LBRA depth)::input) = do 
    let inbrackets = List.takeWhile (\x => x/= RBRA depth) input
    let (lhand, (_ :: rhand)) = break (\x => x == COMMA depth) inbrackets
    parsedL <- parseValue' p1 lhand
    parsedR <- parseValue' p2 rhand
    pure (parsedL, parsedR)
parseValue' SString input = case takeWhile isChar input of
        chars => Just $ pack $ ((\(CHAR x) => x) <$> chars)
    where 
        isChar : Token -> Bool
        isChar (CHAR _) = True
        isChar _ = False

parseValue : (schema: Schema) -> String -> Maybe(SchemaType schema)
parseValue schema string = parseValue' schema (Tokenize (unpack string) 0)


processInput : DataStore -> String -> Maybe (String, DataStore)
processInput store inp
    = case parse inp of
    Nothing => Just ("Invalid command\n", store)
    Just (Add x) => 
        case parseValue (schema store) x of 
             Nothing => Just ("Invalid type", store)
             Just (v) => Just ("", addToStore store v)
    Just (Get x) => case (tryIndex x (items store)) of
            Nothing => Just ("nope ", store)
            Just item => Just (stringify (schema store) item, store)
    Just(Size) => Just(show (size store), store)
    Just(Search x) => Just(searchResultToString store (search store x), store)
    Just Quit => Nothing

  
main : IO ()
main = replWith (MkData (SInt .+. SString) _ []) "Command:" processInput
