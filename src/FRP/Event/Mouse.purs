module FRP.Event.Mouse
  ( Mouse(..)
  , Position
  , Buttons
  , readPosition
  , readButtons
  , getMouse
  , disposeMouse
  , down'
  , down
  , up'
  , up
  , withPosition
  , withButtons
  ) where

import Prelude

import Ansi.Codes (EscapeCode(..))
import Data.Foldable (traverse_)
import Data.Maybe (Maybe(..))
import Data.Newtype (wrap)
import Data.Set as Set
import Effect (Effect)
import Effect.Ref as Ref
import FRP.Event (Event, makeEventE)
import Safe.Coerce (coerce)
import Web.Event.EventTarget (addEventListener, eventListener, removeEventListener)
import Web.HTML (window)
import Web.HTML.Window (toEventTarget)
import Web.UIEvent.MouseEvent (button, clientX, clientY, fromEvent)

type Position = { x :: Int, y :: Int }

type Buttons = Set.Set Int
-- | A handle for creating events from the mouse position and buttons.
newtype Mouse = Mouse
  { position :: Ref.Ref (Maybe Position)
  , buttons :: Ref.Ref Buttons
  , dispose :: Effect Unit
  }

readPosition :: Mouse -> Effect (Maybe Position)
readPosition (Mouse { position }) = Ref.read position

readButtons :: Mouse -> Effect Buttons
readButtons (Mouse { buttons }) = Ref.read buttons

-- | Get a handle for working with the mouse.
getMouse :: Effect Mouse
getMouse = do
  position <- Ref.new Nothing
  buttons <- Ref.new Set.empty
  target <- toEventTarget <$> window
  mouseMoveListener <- eventListener \e -> do
    fromEvent e # traverse_ \me ->
      Ref.write (Just { x: clientX me, y: clientY me }) position
  mouseDownListener <- eventListener \e -> do
    fromEvent e # traverse_ \me ->
      Ref.modify (Set.insert (button me)) buttons
  mouseUpListener <- eventListener \e -> do
    fromEvent e # traverse_ \me ->
      Ref.modify (Set.delete (button me)) buttons
  addEventListener (wrap "mousemove") mouseMoveListener false target
  addEventListener (wrap "mousedown") mouseDownListener false target
  addEventListener (wrap "mouseup") mouseUpListener false target
  let
    dispose = do
      removeEventListener (wrap "mousemove") mouseMoveListener false target
      removeEventListener (wrap "mousedown") mouseDownListener false target
      removeEventListener (wrap "mouseup") mouseUpListener false target
  pure (Mouse { position, buttons, dispose })

disposeMouse :: Mouse -> Effect Unit
disposeMouse (Mouse { dispose }) = dispose

-- | Create an `Event` which fires when a mouse button is pressed
down'
  :: forall a
   . ((a -> Effect Unit) -> Int -> Effect Unit)
  -> Effect
       { event :: Event a
       , unsubscribe :: Effect Unit
       }
down' f = makeEventE \k -> do
  target <- toEventTarget <$> window
  mouseDownListener <- eventListener \e -> do
    fromEvent e # traverse_ \me -> f k (button me)
  addEventListener (wrap "mousedown") mouseDownListener false target
  pure (removeEventListener (wrap "mousedown") mouseDownListener false target)

down
  :: Effect
       { event :: Event Int
       , unsubscribe :: Effect Unit
       }
down = down' (($))

-- | Create an `Event` which fires when a mouse button is released
up'
  :: forall a
   . ((a -> Effect Unit) -> Int -> Effect Unit)
  -> Effect
       { event :: Event a
       , unsubscribe :: Effect Unit
       }
up' f = makeEventE \k -> do
  target <- toEventTarget <$> window
  mouseUpListener <- eventListener \e -> do
    fromEvent e # traverse_ \me -> f k (button me)
  addEventListener (wrap "mouseup") mouseUpListener false target
  pure (removeEventListener (wrap "mouseup") mouseUpListener false target)

up
  :: Effect
       { event :: Event Int
       , unsubscribe :: Effect Unit
       }
up = up' (($))

-- | Create an event which also returns the current mouse position.
withPosition
  :: forall a
   . Mouse
  -> ({ value :: a, pos :: Maybe { x :: Int, y :: Int } } -> Effect Unit)
  -> a
  -> Effect Unit
withPosition (Mouse { position }) = go
  where
  go f value = do
    pos <- Ref.read position
    f { value, pos }

withButtons
  :: forall a
   . Mouse
  -> ({ value :: a, buttons :: Set.Set Int } -> Effect Unit)
  -> a
  -> Effect Unit
withButtons (Mouse { buttons }) = go
  where
  go f value = do
    buttonsValue <- Ref.read buttons
    f { value, buttons: buttonsValue }
