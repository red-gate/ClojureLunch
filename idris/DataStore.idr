module Main
import Data.Vect

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



main : IO ()
main = replWith 0 "Data" sumInputs
