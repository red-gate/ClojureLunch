module Main where

import Phoenix

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (log, CONSOLE)
import DOM.HTML.Event.EventTypes (timeout)
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
  p2 <- receive p "ok" (\p d -> log "Joined lobby")
  log "Hello s"





