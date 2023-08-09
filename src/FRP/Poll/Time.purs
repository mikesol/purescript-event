module FRP.Poll.Time
  ( instant
  , seconds
  ) where

import Prelude

import Data.DateTime.Instant (Instant, unInstant)
import Data.Time.Duration (Seconds, toDuration)
import FRP.Poll (Poll, poll)
import FRP.Event.Time (withTime)

-- | Get the current time in milliseconds since the epoch.
instant :: Poll Instant
instant = poll \e -> map (\{ value, time: t } -> value t) (withTime e)

-- | Get the current time in seconds since the epoch.
seconds :: Poll Seconds
seconds = map (toDuration <<< unInstant) instant
