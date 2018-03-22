module Main
import Data.Vect
import Data.Fin

data DataStore : Type where
    MkData : (size : Nat) ->
    (items : Vect size String) -> DataStore

size : DataStore -> Nat
size (MkData size' items') = size'

items : (store : DataStore) -> Vect (size store) String
items (MkData size' items') = items'


addToStore : DataStore -> String -> DataStore
addToStore (MkData size items') newitem = MkData _ (addToData items')
 where
    addToData : Vect old String -> Vect (S old) String
    addToData [] = [newitem]
    addToData (item :: items) = item :: addToData items



sumInputs : Integer -> String -> Maybe (String, Integer)
sumInputs x y = let v = cast y in
        if v < 0 
            then Nothing
            else let newVal = v + x in Just ("New = " ++ (show newVal), newVal)

data Command = 
    Add String
    | Get Integer
    | Quit

parseCommand : (cmd : String) -> (args : String) ->
    Maybe Command
parseCommand "add" str = Just (Add str)
parseCommand "get" val = case all isDigit (unpack val) of
    False => Nothing
    True => Just (Get (cast val))
parseCommand "quit" "" = Just Quit
parseCommand _ _ = Nothing

parse : (input : String) -> Maybe Command
parse input = case span (/= ' ') input of 
    (cmd, arg) => parseCommand cmd (ltrim arg)

tryIndex : Integer -> Vect n a -> Maybe a
tryIndex {n} x xs = case integerToFin x n of
                        Nothing => Nothing
                        (Just y) => Just(Data.Vect.index y xs)

processInput : DataStore -> String -> Maybe (String, DataStore)
processInput store inp
    = case parse inp of
    Nothing => Just ("Invalid command\n", store)
    Just (Add x) => Just ("", addToStore store x)
    Just (Get x) => case (tryIndex x (items store)) of
            Nothing => Just ("nope ", store)
            Just item => Just (item, store)
    Just Quit => Nothing

    
main : IO ()
main = replWith (MkData _ []) "Command:" processInput
