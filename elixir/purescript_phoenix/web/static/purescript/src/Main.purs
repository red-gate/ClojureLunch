module Main where

import Phoenix

import CSS (background, backgroundColor, color, green, rgb)
import CSS.Transform (offsetLeft)
import Control.Monad.Eff (Eff, forE)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (log, CONSOLE)
import Control.Monad.Eff.Exception (EXCEPTION)
import Control.Monad.Eff.Random (RANDOM, randomInt)
import Control.Monad.Error.Class (catchError)
import Control.Monad.Except (runExcept, runExceptT)
import DOM.Event.Event (currentTarget)
import DOM.Event.MouseEvent (MouseEvent, eventToMouseEvent, screenX, screenY, clientX, clientY)
import DOM.HTML.Event.EventTypes (timeout)
import DOM.HTML.HTMLElement (offsetTop)
import DOM.Node.Node (parentElement)
import Data.Array (foldMap, index, length, updateAt)
import Data.Either (Either(..), either)
import Data.FoldableWithIndex (foldMapWithIndex)
import Data.Foreign (Foreign, toForeign, unsafeFromForeign)
import Data.Function (apply)
import Data.Identity (Identity(..))
import Data.Int (toNumber)
import Data.Maybe (Maybe(..), fromJust)
import Data.Monoid (mempty)
import Data.Traversable (foldr, traverse, traverse_)
import Data.Tuple (Tuple(..))
import Data.Unfoldable (replicateA)
import Graphics.Canvas (CANVAS, CanvasElement, Context2D, clearRect, getCanvasElementById, getContext2D)
import Graphics.Drawing (Drawing, black, closed, fillColor, filled, rectangle, render, rotate, scale, shadow, shadowBlur, shadowColor, translate)
import Partial.Unsafe (unsafePartial)
import Prelude hiding (div,join)
import Pux (CoreEffects, EffModel, start)
import Pux.DOM.Events (DOMEvent, onChange, onClick, onMouseMove, targetValue)
import Pux.DOM.HTML (HTML)
import Pux.DOM.HTML.Attributes (style)
import Pux.Renderer.React (renderToDOM)
import Signal.Channel (CHANNEL, subscribe)
import Signal.Channel as Sig
import Text.Smolder.HTML (Html, button, canvas, div, input, li, span, ul)
import Text.Smolder.HTML.Attributes (height, type', value, width, xmlns)
import Text.Smolder.Markup (text, (#!), (!))
-- | Start and render the app
foreign import emptyJsObject :: Foreign

type Coords = { x:: Int, y:: Int}
type AppEffects eff = (canvas :: CANVAS, channel :: CHANNEL, exception :: EXCEPTION, phoenix :: PHOENIX, console :: CONSOLE, random :: RANDOM  | eff)
type State = { 
  msg :: String , 
  chan :: Channel, 
  sortChan :: Channel, 
  msgs :: Array String, 
  coords:: Coords,
  list:: Array(Int),
  ctx :: Context2D
}


main :: forall eff. Eff (AppEffects eff)  Unit
main = do
  mcanvas <- getCanvasElementById "canvas"
  let canvas = unsafePartial (fromJust mcanvas)
  ctx <- getContext2D canvas

  sigChannel <- Sig.channel SendMessage  

  sock <- newSocket "/socket" defaultSocketOptions
  connect sock
  chan <- channel sock ("room:lobby") emptyJsObject
  sortChan <- channel sock ("sorter:all") emptyJsObject
  on chan "new_message" (\c e m -> Sig.send sigChannel (MessageReceived ((unsafeFromForeign m).value) ))
  on chan "mouse_moved" (\c e m -> Sig.send sigChannel (MouseMoveReceived ((unsafeFromForeign m).value)))
  on sortChan "list_update" (\c e m -> Sig.send sigChannel (ListUpdated ((unsafeFromForeign m).value) ))
  p <- join chan
  p2 <- receive p "ok" (\p d -> log "Joined lobby")

  p <- join sortChan
  p2 <- receive p "ok" (\p d -> log "Joined sorter")

  app <- start
    { initialState: {ctx: ctx, list: [], msg: "Hello world", chan: chan, sortChan : sortChan,  msgs: ["one", "two"], coords: { x: 0, y: 0} }
    , view
    , foldp
    , inputs: [ subscribe sigChannel ]
    }

  renderToDOM "#app" app.markup app.input



sendMessage :: forall t eff. Channel -> t -> String -> Eff (AppEffects eff) Unit
sendMessage chan msg t =  do 
  _ <- push chan t $ toForeign {value: msg}
  pure unit

  
data Event = TextUpdated String | SendMessage | MessageReceived String | MouseMoved DOMEvent | MouseMoveReceived Coords | SortList | ListUpdated (Array (Array Int)) | InitList (Array Int)

getMouseEvent :: DOMEvent -> Maybe ({x :: Int, y :: Int})
getMouseEvent e = 
  either (\_ -> Nothing) (Just) $ runExcept $ do
      me <- eventToMouseEvent e
      pure $ {x : (clientX me), y :((clientY me))}
  

  where f _ = TextUpdated "foo" 
    
-- | Return a new state (and effects) from each event
foldp :: ∀ fx. Event -> State -> EffModel State Event (canvas :: CANVAS, random :: RANDOM, console :: CONSOLE, phoenix :: PHOENIX | fx )
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
foldp SortList s = 
    { state: s   , 
    effects: [ do 
                randoms :: Array Int <- liftEff $ replicateA 20 (randomInt 1 20)
                _ <- liftEff $ sendMessage s.sortChan randoms "sort_list"
                pure (Just (InitList randoms)) ] }

foldp (ListUpdated changes) s = let newList = (foldr applyChange s.list changes) in 
  { state : s { 
    list = newList
  }, effects : [do 
                _ <- liftEff $ log ("Changes: " <> show changes)
                _ <- liftEff $ log ("Old: " <> show s.list)
                _ <- liftEff $ log ("New: " <> show newList)
                _ <- liftEff $ drawGraph s.ctx newList
                pure (Just $ MessageReceived $ show newList)]}

foldp (InitList list) s = { state: s { list = list }, effects: []}

applyChange :: Array Int -> Array Int -> Array Int
applyChange c s = unsafePartial $
  let Just value = (index c 0) in
  let Just index = (index c 1) in
  let Just x = updateAt index value s  in
  x

drawGraph :: forall t83.                 
  Context2D                 
  -> Array Int              
     -> Eff                 
          ( canvas :: CANVAS
          | t83             
          )                 
          Unit
drawGraph ctx list = do 
  _ <- clearRect ctx {x: 0.0, y: 0.0, w: 1000.0, h: 1000.0} 
  render ctx (drawList list)

drawList :: Array Int -> Drawing
drawList ls = scale 3.0 3.0 $ foldMapWithIndex valueToLine ls
  where valueToLine i x =  filled (fillColor (rgb 200 (200 - (10*x)) 34)) (rectangle (toNumber i) 0.0 1.0 $ (toNumber x) )

-- | Return markup from the state
view :: State -> HTML Event
view state =
  div do
    button #! onClick (const SortList) $ text "Sort List"
    button #! onClick (const SendMessage) $ text "Send Message"
    input ! type' "text" ! value state.msg #! onChange (\x -> TextUpdated (targetValue x) )
    div do
      ul do
        traverse_ (\x -> li $ text x) state.msgs 