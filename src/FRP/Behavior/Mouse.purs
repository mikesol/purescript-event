module FRP.Behavior.Mouse
  ( position
  , buttons
  ) where

import Prelude

import Data.Maybe (Maybe)
import Data.Set as Set
import Data.Tuple (Tuple(..))
import Effect.Ref as Ref
import FRP.Behavior (Behavior, behavior)
import FRP.Event.Mouse (Mouse(..))

-- | A `Behavior` which reports the current mouse position, if it is known.
position :: Mouse -> Behavior (Maybe { x :: Int, y :: Int })
position (Mouse m) = behavior (pure (Tuple (pure unit) (Ref.read m.position)))

-- | A `Behavior` which reports the mouse buttons which are currently pressed.
buttons :: Mouse -> Behavior (Set.Set Int)
buttons (Mouse m) = behavior (pure (Tuple (pure unit) (Ref.read m.buttons)))
