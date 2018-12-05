import Data.Primitives.Views

record Score where
    constructor MkScore
    correct : Nat
    attempted : Nat

record GameState where
    constructor MkGameState
    score : Score
    difficulty : Nat

data Input = Answer Int
            | QuitCmd

initState : GameState
initState = MkGameState (MkScore 0 0) 12

Show GameState where
    show st = show (correct (score st)) ++ "/" ++
              show (attempted (score st)) ++ "\n" ++
              "Difficulty: " ++ show (difficulty st)

setDifficulty : Nat -> GameState -> GameState
setDifficulty newDiff = record { difficulty = newDiff }

addWrong : GameState -> GameState
addWrong = record { score->attempted $= (+1) }

addCorrect : GameState -> GameState
addCorrect = record { score->correct $= (+1),
                      score->attempted $= (+1) }


data Command : Type -> Type where
    PutStr : String -> Command ()
    GetLine : Command String

    GetRandom : Command Int
    GetGameState : Command GameState
    PutGameState : GameState -> Command ()

    Pure : ty -> Command ty
    Bind : Command a -> (a -> Command b) -> Command b

data ConsoleIO : Type -> Type where
    Quit : a -> ConsoleIO a
    Do : Command a -> (a -> Inf (ConsoleIO b)) -> ConsoleIO b

namespace CommandDo
    (>>=) : Command a -> (a -> Command b) -> Command b
    (>>=) = Bind

namespace ConsoleDo
    (>>=) : Command a -> (a -> Inf (ConsoleIO b)) -> ConsoleIO b
    (>>=) = Do
   
data Fuel = Dry | More (Lazy Fuel)

forever : Fuel
forever = More forever
runCommand : Command a -> IO a
runCommand (PutStr x) = putStr x
runCommand GetLine = getLine
runCommand (Pure val) = pure val
runCommand (Bind c f) = do res <- runCommand c
                           runCommand (f res)    
runCommand (PutGameState x) = ?runCommand_rhs_1
runCommand GetGameState = ?runCommand_rhs_2
runCommand GetRandom = ?runCommand_rhs_3                           

runCommand : Stream Int -> GameState -> Command a ->
                IO (a, Stream Int, GameState)
runCommand rnds state (PutStr x) = do putStr x
                                      pure ((), rnds, state)
runCommand rnds state GetLine = do str <- getLine
                                   pure (str, rnds, state)
runCommand (val :: rnds) state GetRandom
        = pure (getRandom val (difficulty state), rnds, state)
    where
        getRandom : Int -> Int -> Int
        getRandom val max with (divides val max)
            getRandom val 0 | DivByZero = 1
            getRandom ((max * div) + rem) max | (DivBy prf) = abs rem + 1
runCommand rnds state GetGameState
    = pure (state, rnds, state)
runCommand rnds state (PutGameState newState)
    = pure ((), rnds, newState)
runCommand rnds state (Pure val)
    = pure (val, rnds, state)
runCommand rnds state (Bind c f)
    = do (res, newRnds, newState) <- runCommand rnds state c
         runCommand newRnds newState (f res)    


run : Fuel -> ConsoleIO a -> IO (Maybe a)
run fuel (Quit val) = do pure (Just val)
run (More fuel) (Do c f) = do res <- runCommand c
                              run fuel (f res)
run Dry p = pure Nothing                           


mutual
    readInput : (prompt : String) -> Command Input
    readInput prompt = do PutStr prompt
                          answer <- GetLine
                          if toLower answer == "quit"
                          then Pure QuitCmd
                          else Pure (Answer (cast answer))
    correct : ConsoleIO GameState
    correct = do PutStr "Correct!\n"
                 st <- GetGameState
                 PutGameState (addCorrect st)
                 quiz
    wrong : Int -> ConsoleIO GameState
    wrong ans = do PutStr ("Wrong, the answer is " ++ show ans ++ "\n")
                   st <- GetGameState
                   PutGameState (addWrong st)
                   quiz
    quiz : ConsoleIO GameState
    quiz = do num1 <- GetRandom
              num2 <- GetRandom
              st <- GetGameState
              PutStr (show st ++ "\n")
              input <- readInput (show num1 ++ " * " ++ show num2 ++ "? ")
              case input of
                   Answer answer => if answer == num1 * num2
                                    then correct
                                    else wrong (num1 * num2)
                   QuitCmd => Quit st