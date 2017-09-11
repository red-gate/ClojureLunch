module Main where

import Phoenix

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (log, CONSOLE)
import DOM.HTML.Event.EventTypes (timeout)
import Data.Foreign (Foreign, toForeign, unsafeFromForeign)
import Prelude hiding (div,join)

-- | Start and render the app
foreign import emptyJsObject :: Foreign

type AppEffects eff = (phoenix :: PHOENIX, console :: CONSOLE | eff)

main :: forall eff. Eff (AppEffects eff)  Unit
main = do 
  sock <- newSocket "/socket" defaultSocketOptions
  connect sock
  chan <- channel sock "room:lobby" emptyJsObject
  on chan "new_message" (\c e m -> log (unsafeFromForeign m))
  p <- join chan
  p2 <- receive p "ok" (\p d -> log "Joined lobby")
  p3 <- push chan "new_message" $ toForeign {value: "I'm here"}
  log "Hello s"






