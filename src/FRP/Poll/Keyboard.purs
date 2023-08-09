module FRP.Poll.Keyboard
  ( keys
  , key
  ) where

import Prelude

import Data.Set as Set
import FRP.Poll (Poll, poll)
import FRP.Event.Keyboard (Keyboard, withKeys)

-- | A `Poll` which reports the keys which are currently pressed.
keys :: Keyboard -> Poll (Set.Set String)
keys keyboard = poll \e -> map (\{ value, keys: ks } -> value (Set.fromFoldable ks)) (withKeys keyboard e)

-- | A `Poll` which reports whether a specific key is currently pressed.
key :: Keyboard -> String -> Poll Boolean
key keyboard k = Set.member k <$> keys keyboard
