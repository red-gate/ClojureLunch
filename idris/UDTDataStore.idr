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
stringify (z1 .+. z2) (x,y) = stringify z1 x ++ stringify z2 y

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

                       
parseValue : (schema: Schema) -> String -> Maybe(SchemaType schema, String)
parseValue SInt input = case partition isDigit $ unpack input of
    (digits, tail) => Just ((cast $ pack digits) , (pack tail))

{-
processInput : DataStore -> String -> Maybe (String, DataStore)
processInput store inp
    = case parse inp of
    Nothing => Just ("Invalid command\n", store)
    Just (Add x) => Just ("", addToStore store x)
    Just (Get x) => case (tryIndex x (items store)) of
            Nothing => Just ("nope ", store)
            Just item => Just (item, store)
    Just(Size) => Just(show (size store), store)
    Just(Search x) => Just(searchResultToString(search store x), store)
    Just Quit => Nothing


  
main : IO ()
main = replWith (MkData _ []) "Command:" processInput
-}