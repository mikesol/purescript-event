module FRP.Behavior.Keyboard
  ( keys
  , key
  ) where

import Prelude

import Data.Set as Set
import FRP.Behavior (Behavior, stRefToBehavior)
import FRP.Event.Keyboard (Keyboard(..))

-- | A `Behavior` which reports the keys which are currently pressed.
keys :: Keyboard -> Behavior (Set.Set String)
keys (Keyboard k) = stRefToBehavior k.keys

-- | A `Behavior` which reports whether a specific key is currently pressed.
key :: Keyboard -> String -> Behavior Boolean
key keyboard k = Set.member k <$> keys keyboard
