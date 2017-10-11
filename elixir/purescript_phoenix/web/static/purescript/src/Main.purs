module Main where

import Phoenix

import CSS (background, backgroundColor, color, green)
import CSS.Transform (offsetLeft)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (log, CONSOLE)
import Control.Monad.Eff.Exception (EXCEPTION)
import Control.Monad.Error.Class (catchError)
import Control.Monad.Except (runExcept, runExceptT)
import DOM.Event.Event (currentTarget)
import DOM.Event.MouseEvent (MouseEvent, eventToMouseEvent, screenX, screenY, clientX, clientY)
import DOM.HTML.Event.EventTypes (timeout)
import DOM.HTML.HTMLElement (offsetTop)
import DOM.Node.Node (parentElement)
import Data.Either (Either(..), either)
import Data.Foreign (Foreign, toForeign, unsafeFromForeign)
import Data.Identity (Identity(..))
import Data.Maybe (Maybe(..))
import Data.Traversable (traverse, traverse_)
import Data.Tuple (Tuple(..))
import Prelude hiding (div,join)
import Pux (CoreEffects, EffModel, start)
import Pux.DOM.Events (DOMEvent, onChange, onClick, onMouseMove, targetValue)
import Pux.DOM.HTML (HTML)
import Pux.DOM.HTML.Attributes (style)
import Pux.Renderer.React (renderToDOM)
import Signal.Channel (CHANNEL, subscribe)
import Signal.Channel as Sig
import Text.Smolder.HTML (Html, button, canvas, div, input, li, span, ul)
import Text.Smolder.HTML.Attributes (height, type', value, width)
import Text.Smolder.Markup (text, (#!), (!))

-- | Start and render the app
foreign import emptyJsObject :: Foreign

type Coords = { x:: Int, y:: Int}
type AppEffects eff = (channel :: CHANNEL, exception :: EXCEPTION, phoenix :: PHOENIX, console :: CONSOLE | eff)
type State = { msg :: String , chan :: Channel, msgs :: Array String, coords:: Coords }


main :: forall eff. Eff (AppEffects eff)  Unit
main = do

  sigChannel <- Sig.channel SendMessage  

  sock <- newSocket "/socket" defaultSocketOptions
  connect sock
  chan <- channel sock ("room:lobby") emptyJsObject
  on chan "new_message" (\c e m -> Sig.send sigChannel (MessageReceived ((unsafeFromForeign m).value) ))
  on chan "mouse_moved" (\c e m -> Sig.send sigChannel (MouseMoveReceived ((unsafeFromForeign m).value)))
  p <- join chan
  p2 <- receive p "ok" (\p d -> log "Joined lobby")

  app <- start
    { initialState: {msg: "Hello world", chan: chan, msgs: ["one", "two"], coords: { x: 0, y: 0} }
    , view
    , foldp
    , inputs: [ subscribe sigChannel ]
    }

  renderToDOM "#app" app.markup app.input



sendMessage :: forall t eff. Channel -> t -> String -> Eff (AppEffects eff) Unit
sendMessage chan msg t =  do 
  _ <- push chan t $ toForeign {value: msg}
  pure unit
  
data Event = TextUpdated String | SendMessage | MessageReceived String | MouseMoved DOMEvent | MouseMoveReceived Coords

getMouseEvent :: DOMEvent -> Maybe ({x :: Int, y :: Int})
getMouseEvent e = 
  either (\_ -> Nothing) (Just) $ runExcept $ do
      me <- eventToMouseEvent e
      pure $ {x : (clientX me), y :((clientY me))}
  

  where f _ = TextUpdated "foo" 
    
-- | Return a new state (and effects) from each event
foldp :: ∀ fx. Event -> State -> EffModel State Event ( console :: CONSOLE, phoenix :: PHOENIX | fx )
foldp SendMessage s = { state: s { msg = "" }  , 
    effects: [ do 
                _ <- liftEff $ sendMessage  s.chan s.msg "new_message"
                pure Nothing] }
foldp (TextUpdated msg) s = { state: s { msg = msg }, effects: [] }
foldp (MessageReceived msg) s = { state: s { msgs = (append s.msgs [msg])}, effects: [] }
foldp (MouseMoved event) s = let pos = getMouseEvent event in {state: s, effects: [do 
                case pos of 
                  Just coords -> do 
                    _ <- liftEff $ sendMessage  s.chan coords "mouse_moved"
                    pure Nothing
                  Nothing -> pure Nothing]}
foldp (MouseMoveReceived c) s = { state: s { coords = c } , effects: [do 
                _ <- liftEff $ log (show c.x)
                pure Nothing]}
-- | Return markup from the state
view :: State -> HTML Event
view state =
  div do
    button #! onClick (const SendMessage) $ text "Send Message"
    input ! type' "text" ! value state.msg #! onChange (\x -> TextUpdated (targetValue x) )
    div do
      ul do
        traverse_ (\x -> li $ text x) state.msgs 
    canvas ! width (show (state.coords.x + 30)) ! height (show (state.coords.y - 5)) ! style 
      do
        backgroundColor green
      #! onMouseMove (\x -> MouseMoved x) $ 
      do pure unit





























