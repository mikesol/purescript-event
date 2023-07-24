module FRP.Behavior.Time
  ( instant
  , seconds
  ) where

import Prelude

import Data.DateTime.Instant (Instant, unInstant)
import Data.Time.Duration (Seconds, toDuration)
import Data.Tuple (Tuple(..))
import Effect.Now (now)
import FRP.Behavior (Behavior, behavior)

-- | Get the current time in milliseconds since the epoch.
instant :: Behavior Instant
instant = behavior (pure (Tuple (pure unit) now))

-- | Get the current time in seconds since the epoch.
seconds :: Behavior Seconds
seconds = map (toDuration <<< unInstant) instant
