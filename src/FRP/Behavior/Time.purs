module FRP.Behavior.Time
  ( instant
  , seconds
  ) where

import Prelude

import Control.Lazy (fix)
import Data.DateTime.Instant (Instant, unInstant)
import Data.DateTime.Instant as Instant
import Data.Maybe (Maybe(..))
import Data.Time.Duration (Milliseconds(..), Seconds, toDuration)
import Data.Tuple (Tuple(..))
import Effect.Now (now)
import FRP.Behavior (Behavior, behavior)

-- | Get the current time in milliseconds since the epoch.
instant :: Behavior Instant
instant = behavior
  ( fix \go e -> case Instant.instant $ Milliseconds 0.0 of
      Nothing -> go e
      Just x -> pure x
  )
  (pure (Tuple (pure unit) now))

-- | Get the current time in seconds since the epoch.
seconds :: Behavior Seconds
seconds = map (toDuration <<< unInstant) instant
