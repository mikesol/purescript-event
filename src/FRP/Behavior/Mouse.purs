module FRP.Behavior.Mouse
  ( position
  , buttons
  ) where


import Data.Maybe (Maybe)
import Data.Set as Set
import FRP.Behavior (Behavior, refToBehavior)
import FRP.Event.Mouse (Mouse(..))

-- | A `Behavior` which reports the current mouse position, if it is known.
position :: Mouse -> Behavior (Maybe { x :: Int, y :: Int })
position (Mouse m) = refToBehavior m.position

-- | A `Behavior` which reports the mouse buttons which are currently pressed.
buttons :: Mouse -> Behavior (Set.Set Int)
buttons (Mouse m) = refToBehavior m.buttons
