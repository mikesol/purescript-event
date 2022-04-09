module FRP.Event.Legacy where

import Prelude

import Control.Alternative (class Alt, class Alternative, class Plus)
import Control.Apply (lift2)
import Data.Compactable (class Compactable)
import Effect (Effect)
import FRP.Event (class Filterable)
import FRP.Event as Class
import FRP.Event as Event
import FRP.Event.Class (biSampleOn)

-- | This is the same as `FRP.Event` but with a (somewhat questionable) `Apply` and `Applicative` instance.

newtype Event a = Event (Event.Event a)

derive newtype instance functorEvent :: Functor Event
derive newtype instance compactableEvent :: Compactable Event
derive newtype instance filterableEvent :: Filterable Event
instance applyEvent :: Apply Event where
  apply = flip biSampleOn

instance applicativeEvent :: Applicative Event where
  pure = Event <<< Event.bang

derive newtype instance altEvent :: Alt Event
derive newtype instance plusEvent :: Plus Event
instance alternativeEvent :: Alternative Event
instance semigroupEvent :: Semigroup a => Semigroup (Event a) where
  append = lift2 append

instance monoidEvent :: Monoid a => Monoid (Event a) where
  mempty = pure mempty

derive newtype instance isEvent :: Class.IsEvent Event

-- | Subscribe to an `Event` by providing a callback.
-- |
-- | `subscribe` returns a canceller function.
subscribe
  :: forall a
   . Event a
  -> (a -> Effect Unit)
  -> Effect (Effect Unit)
subscribe (Event e) = Event.subscribe e

-- | Make an `Event` from a function which accepts a callback and returns an
-- | unsubscription function.
-- |
-- | Note: you probably want to use `create` instead, unless you need explicit
-- | control over unsubscription.
makeEvent
  :: forall a
   . ((a -> Effect Unit) -> Effect (Effect Unit))
  -> Event a
makeEvent = Event <<< Event.makeEvent

type EventIO a =
  { event :: Event a
  , push :: a -> Effect Unit
  }

-- | Create an event and a function which supplies a value to that event.
create
  :: forall a
   . Effect (EventIO a)
create = Event.create <#> \i -> i { event = Event i.event }
