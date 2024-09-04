module FRP.Event.Time
  ( interval
  , interval'
  , throttle
  , withTime
  , withDelay
  , withDelay'
  ) where

import Prelude

import Data.Compactable (compact)
import Data.DateTime.Instant (Instant, unInstant)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Data.Time.Duration (Milliseconds)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Aff.AVar as Avar
import Effect.Class (liftEffect)
import Effect.Now (now)
import Effect.Timer (TimeoutId, clearInterval, setInterval, setTimeout)
import FRP.Event

-- | Create an event which reports the current time in milliseconds since the epoch.
-- | Since all You will see are hot, it's not possible to write function `addTime :: Event a -> Event { value a, time :: Instant }`
-- | Instead, use this function with `push`, e.g. `withTime push 1`
withTime :: forall a. ({ value :: a, time :: Instant } -> Effect Unit) -> a -> Effect Unit
withTime = go
  where
  go f value = do
    time <- now
    f { time, value }

withDelay' :: forall a. (a -> Int) -> (Either TimeoutId (Tuple TimeoutId a) -> Effect Unit) -> a -> Effect Unit
withDelay' nf = go
  where
  go f value = launchAff_ do
    tid <- Avar.empty
    o <- liftEffect $ setTimeout (nf value) $ launchAff_ do
      t <- Avar.read tid
      liftEffect $ f (Right (Tuple t value))
    Avar.put o tid
    liftEffect $ f (Left o)

withDelay :: forall a. Int -> (Either TimeoutId (Tuple TimeoutId a) -> Effect Unit) -> a -> Effect Unit
withDelay = withDelay' <<< const

-- | Create an event which fires every specified number of milliseconds.
interval'
  :: forall a
   . ((a -> Effect Unit) -> Instant -> Effect Unit)
  -> Int
  -> Effect { event :: Event a, unsubscribe :: Effect Unit }
interval' f n = makeEventE \k -> do
  id <- setInterval n do
    time <- now
    f k time
  pure (clearInterval id)

interval
  :: Int
  -> Effect
       { event :: Event Instant
       , unsubscribe :: Effect Unit
       }
interval = interval' identity

throttle :: forall a. Milliseconds -> Event { time :: Instant, value :: a } -> Event { time :: Instant, value :: a }
throttle period = compact <<< mapAccum go Nothing
  where
  go Nothing { time, value } = Tuple (Just time) (Just { time, value })
  go (Just lastTime) { time, value } =
    if unwrap (unInstant time) - unwrap (unInstant lastTime) > unwrap period then Tuple (Just time) (Just { time, value })
    else Tuple (Just lastTime) Nothing
