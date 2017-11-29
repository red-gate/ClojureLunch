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
import Data.Int (floor, fromString, toNumber)
import Data.List ((!!), (:))
import Data.List (List, head, singleton)
import Data.List as L
import Data.Maybe (Maybe(..), fromJust)
import Data.Monoid (mempty)
import Data.Traversable (foldr, traverse, traverse_)
import Data.Tuple (Tuple(..))
import Data.Unfoldable (replicateA)
import Graphics.Canvas (CANVAS, CanvasElement, Context2D, clearRect, getCanvasElementById, getContext2D)
import Graphics.Drawing (Drawing, black, closed, fillColor, filled, rectangle, render, rotate, scale, shadow, shadowBlur, shadowColor, translate)
import Messaging as Chat
import Partial.Unsafe (unsafePartial)
import Prelude hiding (div,join)
import Pux (CoreEffects, EffModel, start)
import Pux.DOM.Events (DOMEvent, onChange, onClick, onMouseMove, onMouseOver, targetValue)
import Pux.DOM.HTML (HTML, mapEvent)
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
  chatState :: Chat.State,
  sortChan :: Channel,
  coords:: Coords,
  list:: List (Array Int),
  ctx :: Context2D,
  graphPos :: Int,
  listSize :: Int
}


main :: forall eff. Eff (AppEffects eff)  Unit
main = do
  mcanvas <- getCanvasElementById "canvas"
  let canvas = unsafePartial (fromJust mcanvas)
  ctx <- getContext2D canvas

  sigChannel <- Sig.channel ( ChatEvent $  Chat.SendMessage)

  sock <- newSocket "/socket" defaultSocketOptions
  connect sock
  chan <- channel sock ("room:lobby") emptyJsObject
  sortChan <- channel sock ("sorter:all") emptyJsObject
  on chan "new_message" (\c e m -> Sig.send sigChannel (ChatEvent $ Chat.MessageReceived ((unsafeFromForeign m).value) ))
  on sortChan "list_update" (\c e m -> Sig.send sigChannel (ListUpdated ((unsafeFromForeign m).value) ))
  p <- join chan
  p2 <- receive p "ok" (\p d -> log "Joined lobby")

  p <- join sortChan
  p2 <- receive p "ok" (\p d -> log "Joined sorter")

  app <- start
    { initialState: {chatState: Chat.initial chan,  listSize: 10, ctx: ctx, list: mempty, sortChan : sortChan,  coords: { x: 0, y: 0}, graphPos: 0 }
    , view
    , foldp
    , inputs: [ subscribe sigChannel ]
    }

  renderToDOM "#app" app.markup app.input



sendMessage :: forall t eff. Channel -> t -> String -> Eff (AppEffects eff) Unit
sendMessage chan msg t =  do 
  _ <- push chan t $ toForeign {value: msg}
  pure unit

  
data Event = ChatEvent Chat.Event  | SortList | ListUpdated (Array (Array Int)) | InitList (Array Int) | Draw (Array Int) | ScrollCanvas Int | ListSizeUpdated Int

getMouseEvent :: DOMEvent -> Maybe ({x :: Int, y :: Int})
getMouseEvent e = 
  either (\_ -> Nothing) (Just) $ runExcept $ do
      me <- eventToMouseEvent e
      pure $ {x : (clientX me), y :((clientY me))}
  

  where f _ = Chat.TextUpdated "foo" 
    
-- | Return a new state (and effects) from each event
foldp :: ∀ fx. Event -> State -> EffModel State Event (canvas :: CANVAS, random :: RANDOM, console :: CONSOLE, phoenix :: PHOENIX | fx )
foldp (ChatEvent event) s =
   let c = Chat.foldp event s.chatState
   in {state: s {chatState = c.state},  effects: ((<$>)((<$>)ChatEvent) ) <$> c.effects}   
foldp (ScrollCanvas p) s =
  let pos = max 0 $min p $L.length s.list - 1 
  in
    { state: s { graphPos = pos }, effects: [
      do 
        _ <- liftEff $ drawGraph s.ctx $unsafePartial fromJust (s.list !! pos) 
        pure Nothing
    ] }
foldp (ListSizeUpdated msg) s = { state: s { listSize = max 1 msg }, effects: [] }
foldp SortList s = 
    { state: s  , 
    effects: [ do 
                randoms :: Array Int <- liftEff $ replicateA s.listSize (randomInt 1 s.listSize)
                _ <- liftEff $ sendMessage s.sortChan randoms "sort_list"
                pure (Just (InitList randoms)) ] }

foldp (ListUpdated changes) s = let newList = (foldr applyChange (unsafePartial $ fromJust $ head s.list) changes) 
  in 
  { state : s { 
    list = newList : s.list
  }, effects: [do 
    _ <- liftEff $ drawGraph s.ctx newList
    pure Nothing
  ]}

foldp (InitList list) s = { state: s { list = singleton list }, effects: []}
foldp (Draw l) s = {state: s, effects: [do 
    _ <- liftEff $ drawGraph s.ctx l
    pure Nothing
  ]}

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
  let scf = 100.0 / (toNumber $ length list)
  _ <- clearRect ctx {x: 0.0, y: 0.0, w: 1000.0, h: 1000.0} 
  render ctx (scale scf scf $ drawList (2 * (floor scf)) list )

drawList :: Int -> Array Int -> Drawing
drawList cf ls = foldMapWithIndex valueToLine ls
  where valueToLine i x =  filled (fillColor (rgb 200 (200 - (cf *x)) 34)) (rectangle (toNumber i) 0.0 1.0 $ (toNumber x) )


-- | Return markup from the state
view :: State -> HTML Event
view state =
  div do
    input ! type' "number" ! value (show state.listSize) #! onChange (\x -> ListSizeUpdated (unsafePartial$ fromJust$ fromString(targetValue x) ) )
    button #! onClick (const SortList) $ text "Sort List"
    input ! type' "number" ! value (show state.graphPos) #! onChange (\x -> ScrollCanvas (unsafePartial$ fromJust$ fromString(targetValue x) ) )
    div do
      ul do
        traverse_ (\x -> li #! onMouseOver (const $ Draw x) $ text (show x)) state.list 
    
    mapEvent ChatEvent $ Chat.view (state.chatState)








