module Main where

import Phoenix

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (log, CONSOLE)
import Control.Monad.Eff.Exception (EXCEPTION)
import DOM.HTML.Event.EventTypes (timeout)
import Data.Foreign (Foreign, toForeign, unsafeFromForeign)
import Data.Maybe (Maybe(..))
import Data.Traversable (traverse, traverse_)
import Prelude hiding (div,join)
import Pux (CoreEffects, EffModel, start)
import Pux.DOM.Events (onChange, onClick, targetValue)
import Pux.DOM.HTML (HTML)
import Pux.Renderer.React (renderToDOM)
import Signal.Channel (CHANNEL, subscribe)
import Signal.Channel as Sig
import Text.Smolder.HTML (Html, button, div, input, span, ul, li)
import Text.Smolder.HTML.Attributes (type', value)
import Text.Smolder.Markup (text, (#!), (!))

-- | Start and render the app
foreign import emptyJsObject :: Foreign

type AppEffects eff = (channel :: CHANNEL, exception :: EXCEPTION, phoenix :: PHOENIX, console :: CONSOLE | eff)
type State = { msg :: String , chan :: Channel, msgs :: Array String }

main :: forall eff. Eff (AppEffects eff)  Unit
main = do

  sigChannel <- Sig.channel SendMessage  

  sock <- newSocket "/socket" defaultSocketOptions
  connect sock
  chan <- channel sock "room:lobby" emptyJsObject
  on chan "new_message" (\c e m -> Sig.send sigChannel (MessageReceived ((unsafeFromForeign m).value) ))
  p <- join chan
  p2 <- receive p "ok" (\p d -> log "Joined lobby")

  app <- start
    { initialState: {msg: "Hello world", chan: chan, msgs: ["one", "two"] }
    , view
    , foldp
    , inputs: [ subscribe sigChannel ]
    }

  renderToDOM "#app" app.markup app.input



sendMessage :: forall eff. Channel -> String -> Eff (AppEffects eff) Unit
sendMessage chan msg =  do 
  _ <- push chan "new_message" $ toForeign {value: msg}
  pure unit
  
data Event = TextUpdated String | SendMessage | MessageReceived String

-- | Return a new state (and effects) from each event
foldp :: ∀ fx. Event -> State -> EffModel State Event ( console :: CONSOLE, phoenix :: PHOENIX | fx )
foldp SendMessage s = { state: s { msg = "" }  , 
    effects: [ do 
                _ <- liftEff $ sendMessage  s.chan s.msg
                pure Nothing] }
foldp (TextUpdated msg) s = { state: s { msg = msg }, effects: [] }
foldp (MessageReceived msg) s = { state: s { msgs = (append s.msgs [msg])}, effects: [] }

-- | Return markup from the state
view :: State -> HTML Event
view state =
  div do
    button #! onClick (const SendMessage) $ text "Send Message"
    input ! type' "text" ! value state.msg #! onChange (\x -> TextUpdated (targetValue x) )
    div do
      ul do
        traverse_ (\x -> li $ text x) state.msgs 
