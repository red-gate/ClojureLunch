data State : (stateType : Type) -> Type -> Type where
    Get : State stateType stateType
    Put : stateType -> State stateType ()
    Pure : ty -> State stateType ty
    Bind : State stateType a -> (a -> State stateType b) -> State stateType b

get : State stateType stateType
get = Get
put : stateType -> State stateType ()
put = Put
pure : ty -> State stateType ty
pure = Pure

(>>=) : State stateType a -> (a -> State stateType b) -> State stateType b
(>>=) = Bind

data Tree a = Empty
            | Node (Tree a) a (Tree a)
testTree : Tree String
testTree = Node (Node (Node Empty "Jim" Empty) "Fred"
                (Node Empty "Sheila" Empty)) "Alice"
                (Node Empty "Bob" (Node Empty "Eve" Empty))

treeLabelWithState : Tree a -> State (Stream labelType) (Tree (labelType, a))
treeLabelWithState Empty = pure Empty
treeLabelWithState (Node left val right)
    = do left_labelled <- treeLabelWithState left
         (this :: rest) <- get
         put rest
         right_labelled <- treeLabelWithState right
         pure (Node left_labelled (this, val) right_labelled)

runState : State stateType a -> (st : stateType) -> (a, stateType)
runState Get st = (st, st)
runState (Put newState) st = ((), newState)
runState (Pure x) st = (x, st)
runState (Bind cmd prog) st = let (val, nextState) = runState cmd st in
                                  runState (prog val) nextState

--    Page 336
-- addIfPositive : Integer -> State Integer Bool
-- addIfPositive val = do when (val > 0) $
-- do current <- get
-- put (current + val)
-- pure (val > 0)
-- addPositives : List Integer -> State Integer Nat
-- addPositives vals = do added <- traverse addIfPositive