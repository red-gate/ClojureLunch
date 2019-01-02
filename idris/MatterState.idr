data Matter = Solid | Liquid | Gas

data MatterCmd : Type ->
  Matter ->
  Matter ->
  Type where
    Melt : MatterCmd () Solid Liquid
    Boil : MatterCmd () Liquid Gas
    Freeze : MatterCmd () Liquid Solid
    Condense : MatterCmd () Gas Liquid
    Pure : ty -> MatterCmd ty state state
    (>>=) : MatterCmd a state1 state2 ->
      (a -> MatterCmd b state2 state3) ->
      MatterCmd b state1 state3

iceSteam : MatterCmd () Solid Gas
iceSteam = do Melt
              Boil

steamIce : MatterCmd () Gas Solid
steamIce = do Condense
              Freeze

-- overMelt : MatterCmd () Solid Gas
-- overMelt = do Melt
--               Melt
