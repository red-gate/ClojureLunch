VendState : Type
VendState = (Nat, Nat)

data Input = COIN
           | VEND
           | CHANGE
           | REFILL Nat

data MachineCmd : Type -> VendState -> VendState -> Type where
    InsertCoin : MachineCmd () (pounds, chocs) (S pounds, chocs)
    Vend : MachineCmd () (S pounds, S chocs) (pounds, chocs)
    GetCoins : MachineCmd () (pounds, chocs) (Z, chocs)
    Refill : (bars : Nat) ->
              MachineCmd () (Z, chocs) (Z, bars + chocs)
    Display : String -> MachineCmd () state state
    GetInput : MachineCmd (Maybe Input) state state
    Pure : ty -> MachineCmd ty state state
    (>>=) : MachineCmd a state1 state2 ->
            (a -> MachineCmd b state2 state3) ->
            MachineCmd b state1 state3
data MachineIO : VendState -> Type where
     Do : MachineCmd a state1 state2 ->
          (a -> Inf (MachineIO state2)) -> MachineIO state1

namespace MachineDo
    (>>=) : MachineCmd a state1 state2 ->
            (a -> Inf (MachineIO state2)) -> MachineIO state1
    (>>=) = Do