module Main where

import Phoenix

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (log, CONSOLE)
import Data.Foreign (Foreign, unsafeFromForeign)
import Prelude hiding (div,join)
-- | Start and render the app
foreign import emptyJsObject :: Foreign

type AppEffects eff = (phoenix :: PHOENIX, console :: CONSOLE | eff)

main :: forall eff. Eff (AppEffects eff)  Unit
main = do 
  sock <- newSocket "/socket" defaultSocketOptions
  connect sock
  chan <- channel sock "room:lobby" emptyJsObject
  p <- join chan
  on chan "ok" (\c e d -> log (unsafeFromForeign d))
  log "Hello s"



