import System.Concurrency.Channels

data Message = Add Nat Nat
data MessagePID = MkMessage PID

data Process : Type -> Type where
     Action : IO a -> Process a
     Pure : a -> Process a
     (>>=):  Process a -> (a -> Process b) -> Process b
     Spawn: Process () -> Process (Maybe MessagePID)
     Request : MessagePID -> Message -> Process (Maybe Nat)
     Respond : (msg : Message -> Process Nat) -> Process (Maybe Message)

run : Process a -> IO a
run (Action x) = x
run (Pure x) = pure x
run (x >>= f) = do n <- run x
                   run (f n)
run (Spawn p) = do Just pid <- spawn (run p)
                      | Nothing => pure Nothing
                   pure (Just (MkMessage pid))
run (Request (MkMessage pid) message) = do Just chan <- connect pid
                                                  | Nothing => pure Nothing
                                           ok <- unsafeSend chan message
                                           if ok then do Just x <- unsafeRecv Nat chan
                                                          | Nothing => pure Nothing
                                                         pure (Just x)
                                                 else pure Nothing
run (Respond calc) = do Just sender <- listen 1
                           | Nothing => pure Nothing
                        Just msg <- unsafeRecv Message sender
                           | Nothing => pure Nothing
                        res <- run (calc msg)
                        unsafeSend sender res
                        pure (Just msg)
adder : Process()
adder = do Respond (\(Add o t) => Pure $ o + t)
           adder

procMain : Process ()
procMain = do Just adder_id <- Spawn adder
                   | Nothing => Action (putStrLn "Fail")
              Just answer <- Request adder_id (Add 3 6)
                  | Nothing => Action (putStrLn "Fail")
              Action (printLn answer)
