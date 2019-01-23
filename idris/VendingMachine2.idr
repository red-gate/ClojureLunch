VendState : Type
VendState = (Nat, Nat)

data Input = COIN
           | VEND
           | CHANGE
           | REFILL Nat

data CoinResult = Inserted | Rejected

data MachineCmd : Type -> VendState -> (ty -> VendState) -> Type where
    InsertCoin : MachineCmd CoinResult (pounds, chocs) (\ res => case res of 
            Inserted => (S pounds, chocs)
            Rejected => (pounds, chocs))
    Vend : MachineCmd () (S pounds, S chocs) (const(pounds, chocs))
    GetCoins : MachineCmd () (pounds, chocs) (const (Z, chocs))
    Refill : (bars : Nat) ->
              MachineCmd () (Z, chocs) (const (Z, bars + chocs))
    Display : String -> MachineCmd () state (const state)
    GetInput : MachineCmd (Maybe Input) state (const state)
    Pure : ty -> MachineCmd ty state (const state)
    (>>=) : MachineCmd a state1 state2_fn ->
        ((res : a) -> MachineCmd b (state2_fn res) state3_fn) ->
        MachineCmd b state1 state3_fn

data MachineIO : VendState -> Type where
     Do : MachineCmd a state1 state2 ->
          (a -> Inf (MachineIO (state2 a))) -> MachineIO state1

namespace MachineDo
    (>>=) : MachineCmd a state1 state2 ->
            (a -> Inf (MachineIO (state2 a))) -> MachineIO state1
    (>>=) = Do