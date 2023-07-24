module FRP.Behavior.Keyboard
  ( keys
  , key
  ) where

import Prelude

import Data.Set as Set
import FRP.Behavior (Behavior)
import FRP.Event.Keyboard (Keyboard(..))
import Test.Main (refToBehavior)

-- | A `Behavior` which reports the keys which are currently pressed.
keys :: Keyboard -> Behavior (Set.Set String)
keys (Keyboard k) = refToBehavior k.keys

-- | A `Behavior` which reports whether a specific key is currently pressed.
key :: Keyboard -> String -> Behavior Boolean
key keyboard k = Set.member k <$> keys keyboard
