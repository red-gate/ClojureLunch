import Control.Monad.State

data Tree a = Empty
            | Node (Tree a) a (Tree a)
testTree : Tree String
testTree = Node (Node (Node Empty "Jim" Empty) "Fred"
                (Node Empty "Sheila" Empty)) "Alice"
                (Node Empty "Bob" (Node Empty "Eve" Empty))

flatten : Tree a -> List a
flatten Empty = []
flatten (Node left val right) = flatten left ++ val :: flatten right

treeLabelWith : Stream labelType -> Tree a ->
                (Stream labelType, Tree (labelType, a))
treeLabelWith lbls Empty = (lbls, Empty)
treeLabelWith lbls (Node left val right)
    = let (lblThis :: lblsLeft, left_labelled) = treeLabelWith lbls left
          (lblsRight, right_labelled) = treeLabelWith lblsLeft right
            in
          (lblsRight, Node left_labelled (lblThis, val) right_labelled)

treeLabel : Tree a -> Tree (Integer, a)
treeLabel tree = snd (treeLabelWith [1..] tree)

treeLabelWithState : Tree a -> State (Stream labelType) (Tree (labelType, a))
treeLabelWithState Empty = pure Empty
treeLabelWithState (Node left val right)
    = do left_labelled <- treeLabelWithState left
         (this :: rest) <- get
         put rest
         right_labelled <- treeLabelWithState right
         pure (Node left_labelled (this, val) right_labelled)


update : (stateType -> stateType) -> State stateType ()
update f = do old_state <- get
              put $ f old_state

new_increase : Nat -> State Nat ()
new_increase x = update (+x)              

countEmpty : Tree a -> State Nat ()
countEmpty Empty = new_increase 1
countEmpty (Node left _ right) =
    do countEmpty left
       countEmpty right

countEmptyNode : Tree a -> State (Nat, Nat) ()       
countEmptyNode Empty = update $ \(e,n) => (e+1, n)
countEmptyNode (Node left _ right) = do
        countEmptyNode left
        countEmptyNode right
        update $ \(e,n) => (e, n+1)
        

