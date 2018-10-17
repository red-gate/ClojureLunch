import Data.Vect
import Data.Vect.Views
import Data.Nat.Views
import Data.List.Views

mergeSortVec : Ord a => Vect n a -> Vect n a
mergeSortVec xs with (splitRec xs)
  mergeSortVec [] | SplitRecNil = []
  mergeSortVec [x] | SplitRecOne = [x]
  mergeSortVec (ys ++ zs) | (SplitRecPair lrec rrec) = merge (mergeSortVec ys | lrec) (mergeSortVec zs | rrec)

--This has a bug because of idris :flame:
toBinary : Nat -> String
toBinary n with (halfRec n)
  toBinary Z | HalfRecZ = ""
  toBinary (x + x) | (HalfRecEven rec) =  (the String (toBinary x | rec)) ++ "0"
  toBinary (S (x + x)) | (HalfRecOdd rec) = (the String  (toBinary x | rec)) ++ "1"

palindrome : List Char -> Bool
palindrome s with (vList s)
  palindrome [] | VNil = True
  palindrome [x] | VOne = True
  palindrome (x :: (xs ++ [y])) | (VCons rec) = x==y && (palindrome xs | rec)
