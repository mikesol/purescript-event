module FRP.Event.Time
  ( interval
  , withTime
  , debounce
  , debounceWith
  , delay
  ) where

import Prelude

import Control.Alt ((<|>))
import Data.DateTime.Instant (Instant, instant, unInstant)
import Data.Foldable (for_)
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Set (Set, singleton, delete)
import Data.Time.Duration (Milliseconds)
import Effect.Now (now)
import Effect.Ref as Ref
import Effect.Timer (TimeoutId, clearInterval, clearTimeout, setInterval, setTimeout)
import FRP.Event (Event, bang, makeEvent, subscribe)
import FRP.Event.Class (fix, gateBy)

-- | Create an event which fires every specified number of milliseconds.
interval :: Int -> Event Instant
interval n = makeEvent \k -> do
  id <- setInterval n do
    time <- now
    k time
  pure (clearInterval id)

-- | Create an event which reports the current time in milliseconds since the epoch.
withTime :: forall a. Event a -> Event { value :: a, time :: Instant }
withTime e = makeEvent \k ->
  subscribe e \value -> do
    time <- now
    k { time, value }

-- | On each event, ignore subsequent events for a given number of milliseconds.
debounce :: forall a. Milliseconds -> Event a -> Event a
debounce period = debounceWith (map { period, value: _ })

-- | Provided an input event and transformation, block the input event for the
-- | duration of the specified period on each output.
debounceWith
  :: forall a b
   . (Event a -> Event { period :: Milliseconds, value :: b })
  -> Event a
  -> Event b
debounceWith process event = fix \allowed ->
  let
    processed :: Event { period :: Milliseconds, value :: b }
    processed = process allowed

    expiries :: Event Instant
    expiries =
      map (\{ time, value } -> fromMaybe time (instant (unInstant time <> value)))
        (withTime (map _.period processed))

    comparison :: forall r. Maybe Instant -> { time :: Instant | r } -> Boolean
    comparison a b = maybe true (_ < b.time) a

    unblocked :: Event { time :: Instant, value :: a }
    unblocked = gateBy comparison (bang Nothing <|> map Just expiries) stamped
  in
    { input: map _.value unblocked
    , output: map _.value processed
    }
  where
  stamped :: Event { time :: Instant, value :: a }
  stamped = withTime event

delay :: forall a. Int -> Event a -> Event a
delay n e =
  makeEvent \k -> do
    tid <- Ref.new (mempty :: Set TimeoutId)
    canceler <-
      subscribe e \a -> do
        localId <- Ref.new (Nothing :: Maybe TimeoutId)
        id <-
          setTimeout n do
            k a
            lid <- Ref.read localId
            maybe (pure unit) (\id -> Ref.modify_ (delete id) tid) lid
        Ref.write (Just id) localId
        Ref.modify_ (append (singleton id)) tid
    pure do
      ids <- Ref.read tid
      for_ ids clearTimeout
      canceler