module Messaging where

import Prelude
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
import Partial.Unsafe (unsafePartial)
import Prelude hiding (div,join)
import Pux (CoreEffects, EffModel, start)
import Pux.DOM.Events (DOMEvent, onChange, onClick, onMouseMove, onMouseOver, targetValue)
import Pux.DOM.HTML (HTML)
import Pux.DOM.HTML.Attributes (style)

import Signal.Channel (CHANNEL, subscribe)

import Text.Smolder.HTML (button, div, input, li, ul)
import Text.Smolder.HTML.Attributes (type', value)
import Text.Smolder.Markup (text, (#!), (!))

type AppEffects eff = (channel :: CHANNEL, exception :: EXCEPTION, phoenix :: PHOENIX, console :: CONSOLE | eff)

data Event = TextUpdated String | SendMessage | MessageReceived String
                        
type State = { msg :: String          
             , chan :: Channel        
             , msgs :: Array String }

foldp :: ∀ fx. Event -> State -> EffModel State Event (console :: CONSOLE, phoenix :: PHOENIX | fx )
foldp SendMessage s = { state: s { msg = "" }  , 
    effects: [ do 
                _ <- liftEff $ sendMessage  s.chan s.msg "new_message"
                pure Nothing] }
foldp (TextUpdated msg) s = { state: s { msg = msg }, effects: [] }
foldp (MessageReceived msg) s = { state: s { msgs = (append s.msgs [msg])}, effects: [] }


sendMessage :: forall t eff. Channel -> t -> String -> Eff (AppEffects eff) Unit
sendMessage chan msg t =  do 
  _ <- push chan t $ toForeign {value: msg}
  pure unit

view state = do
    button #! onClick (const SendMessage) $ text "Send Message"
    input ! type' "text" ! value state.msg #! onChange (\x -> TextUpdated (targetValue x) )
    div do
      ul do
        traverse_ (\x -> li $ text x) state.msgs 

initial :: Channel -> State
initial chan =  {msg: "Hello world", chan: chan, msgs: ["one", "two"]}