module FRP.Poll.Mouse
  ( position
  , buttons
  ) where

import Prelude

import Data.Maybe (Maybe)
import Data.Set as Set
import FRP.Poll (Poll, poll)
import FRP.Event.Mouse (Mouse, withPosition, withButtons)

-- | A `Poll` which reports the current mouse position, if it is known.
position :: Mouse -> Poll (Maybe { x :: Int, y :: Int })
position m = poll \e -> map (\{ value, pos } -> value pos) (withPosition m e)

-- | A `Poll` which reports the mouse buttons which are currently pressed.
buttons :: Mouse -> Poll (Set.Set Int)
buttons m = poll \e -> map (\{ value, buttons: bs } -> value bs) (withButtons m e)
