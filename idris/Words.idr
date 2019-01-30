import Data.Vect
%default total

data WordState : (guesses : Nat) -> (letters : Nat) -> Type where
    MkWordState : (word : String)
              -> (missing : Vect letters Char)
              -> WordState guesses_remaining letters

data GameState : Type where
     NotRunning : GameState
     Running : (guesses : Nat) -> (letters : Nat) -> GameState

letters : String -> List Char
letters str = nub (map toUpper (unpack str))

data GuessResult = Correct | Incorrect

data GameCmd : (ty : Type) -> GameState -> (ty -> GameState) -> Type where
    NewGame : (word : String) ->
              GameCmd () NotRunning (const (Running 6 (length (letters word))))
    Won : GameCmd () (Running (S guesses) 0)
                     (const NotRunning)
    Lost : GameCmd () (Running 0 (S guesses))
                      (const NotRunning)
    Guess : (c : Char) ->
        GameCmd GuessResult
                (Running (S guesses) (S letters))
                (\res => case res of
                    Correct => Running (S guesses) letters
                    Incorrect => Running guesses (S letters))
    ShowState : GameCmd () state (const state)
    Message : String -> GameCmd () state (const state)
    ReadGuess : GameCmd Char state (const state)
    Pure : (res : ty) -> GameCmd ty (state_fn res) state_fn
    (>>=) : GameCmd a state1 state2_fn ->
            ((res : a) -> GameCmd b (state2_fn res) state3_fn) ->
            GameCmd b state1 state3_fn


namespace Loop
    data GameLoop : (ty : Type) -> GameState -> (ty -> GameState) -> Type where
        (>>=) : GameCmd a state1 state2_fn ->
                ((res : a) -> Inf (GameLoop b (state2_fn res) state3_fn)) ->
                GameLoop b state1 state3_fn
        Exit : GameLoop () NotRunning (const NotRunning)

gameLoop : GameLoop () (Running (S guesses) (S letters)) (const NotRunning)

gameLoop {guesses} {letters} = do
    ShowState
    g <- ReadGuess
    ok <- Guess g
    case ok of
        Correct => case letters of 
                Z => do Won 
                        ShowState
                        Exit
                (S k) => do Message "Correct"
                            gameLoop
        Incorrect => case guesses of 
            Z => do Lost
                    ShowState
                    Exit
            (S k) => do Message "Incorrect"
                        gameLoop 
