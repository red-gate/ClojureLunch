
data StackCmd : Type -> Nat -> Nat -> Type where
  Push : Integer -> StackCmd () height (S height)
  Pop : StackCmd Integer (S height) height
  Top : StackCmd Integer (S height) (S height)
  GetStr : StackCmd String height height
  PutStr : String -> StackCmd () height height
  Pure : ty -> StackCmd ty height height
  (>>=) : StackCmd a height1 height2 ->
      (a -> StackCmd b height2 height3) ->
      StackCmd b height1 height3

           -- page 368 
