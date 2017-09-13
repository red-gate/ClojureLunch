module Main where

import Phoenix

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (log, CONSOLE)
import Control.Monad.Eff.Exception (EXCEPTION)
import DOM.HTML.Event.EventTypes (timeout)
import Data.Foreign (Foreign, toForeign, unsafeFromForeign)
import Prelude hiding (div,join)
import Pux (CoreEffects, EffModel, start)
import Pux.DOM.Events (onChange, onClick, targetValue)
import Pux.DOM.HTML (HTML)
import Pux.Renderer.React (renderToDOM)
import Signal.Channel (CHANNEL)
import Text.Smolder.HTML (button, div, span, input)
import Text.Smolder.HTML.Attributes (type', value)
import Text.Smolder.Markup (text, (#!), (!))

-- | Start and render the app
foreign import emptyJsObject :: Foreign

type AppEffects eff = (channel :: CHANNEL, exception :: EXCEPTION, phoenix :: PHOENIX, console :: CONSOLE | eff)
type State = { msg :: String , chan :: Channel }

main :: forall eff. Eff (AppEffects eff)  Unit
main = do

  chan <- createChannel

  app <- start
    { initialState: {msg: "Hello world", chan: chan }
    , view
    , foldp
    , inputs: []
    }

  renderToDOM "#app" app.markup app.input



sendMessage :: forall eff. Channel -> String -> Eff (AppEffects eff) Unit
sendMessage chan msg =  do 
  _ <- push chan "new_message" $ toForeign {value: msg}
  pure unit
  
createChannel :: forall eff. Eff (AppEffects eff) Channel
createChannel = do 
  sock <- newSocket "/socket" defaultSocketOptions
  connect sock
  chan <- channel sock "room:lobby" emptyJsObject
  on chan "new_message" (\c e m -> log (unsafeFromForeign m))
  p <- join chan
  p2 <- receive p "ok" (\p d -> log "Joined lobby")
  pure chan



data Event = TextUpdated String | SendMessage

-- | Return a new state (and effects) from each event
foldp :: ∀ fx. Event -> State -> EffModel State Event fx
foldp SendMessage s = { state: s, effects: [] }
foldp (TextUpdated msg) s = { state: {msg: msg, chan: s.chan }, effects: [] }

-- | Return markup from the state
view :: State -> HTML Event
view state =
  div do
    button #! onClick (const SendMessage) $ text "Send Message"
    input ! type' "text" ! value state.msg #! onChange (\x -> TextUpdated (targetValue x) )







