module FRP.Event
  ( Event
  , EventIO
  , create
  , makeEvent
  , subscribe
  , bang
  , bus
  , memoize
  , module Class
  ) where

import Prelude

import Control.Alternative (class Alt, class Plus)
import Data.Array (deleteBy)
import Data.Compactable (class Compactable)
import Data.Either (Either(..), either, hush)
import Data.Filterable (class Filterable, filterMap)
import Data.Foldable (sequence_, traverse_)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Ref as Ref
import Effect.Unsafe (unsafePerformEffect)
import FRP.Event.Class (class Filterable, class IsEvent, count, filterMap, fix, fold, folded, gate, gateBy, keepLatest, mapAccum, sampleOn, sampleOn_, withLast) as Class
import Unsafe.Reference (unsafeRefEq)

-- | An `Event` represents a collection of discrete occurrences with associated
-- | times. Conceptually, an `Event` is a (possibly-infinite) list of values-and-times:
-- |
-- | ```purescript
-- | type Event a = List { value :: a, time :: Time }
-- | ```
-- |
-- | Events are created from real events like timers or mouse clicks, and then
-- | combined using the various functions and instances provided in this module.
-- |
-- | Events are consumed by providing a callback using the `subscribe` function.
newtype Event a = Event ((a -> Effect Unit) -> Effect (Effect Unit))

instance functorEvent :: Functor Event where
  map f (Event e) = Event \k -> e (k <<< f)

instance compactableEvent :: Compactable Event where
  compact = filter identity
  separate xs =
    { left:
        filter
          ( case _ of
              Left x -> Just x
              Right _ -> Nothing
          )
          xs
    , right:
        filter
          ( case _ of
              Right x -> Just x
              Left _ -> Nothing
          )
          xs
    }

filter' :: forall a. (a → Boolean) → Event a → Event a
filter' f =
  filter
    ( \a -> case f a of
        true -> Just a
        false -> Nothing
    )

instance filterableEvent :: Filterable Event where
  filter = filter'
  filterMap = filter
  partition p xs = { yes: filter' p xs, no: filter' (not <<< p) xs }
  partitionMap f xs =
    { left: filterMap (either Just (const Nothing) <<< f) xs
    , right: filterMap (hush <<< f) xs
    }

instance altEvent :: Alt Event where
  alt (Event f) (Event g) =
    Event \k -> do
      c1 <- f k
      c2 <- g k
      pure (c1 *> c2)

instance plusEvent :: Plus Event where
  empty = Event \_ -> pure (pure unit)

instance eventIsEvent :: Class.IsEvent Event where
  fold = fold
  keepLatest = keepLatest
  sampleOn = sampleOn
  fix = fix
  bang = bang

-- | Fold over values received from some `Event`, creating a new `Event`.
fold :: forall a b. (a -> b -> b) -> Event a -> b -> Event b
fold f (Event e) b =
  Event \k -> do
    result <- Ref.new b
    e \a -> Ref.modify (f a) result >>= k

-- | Create an `Event` which only fires when a predicate holds.
filter :: forall a b. (a -> Maybe b) -> Event a -> Event b
filter p (Event e) =
  Event \k ->
    e \a -> case p a of
      Just y -> k y
      Nothing -> pure unit

-- | Create an `Event` which samples the latest values from the first event
-- | at the times when the second event fires.
sampleOn :: forall a b. Event a -> Event (a -> b) -> Event b
sampleOn (Event e1) (Event e2) =
  Event \k -> do
    latest <- Ref.new Nothing
    c1 <-
      e1 \a -> do
        Ref.write (Just a) latest
    c2 <-
      e2 \f -> do
        Ref.read latest >>= traverse_ (k <<< f)
    pure (c1 *> c2)

-- | Flatten a nested `Event`, reporting values only from the most recent
-- | inner `Event`.
keepLatest :: forall a. Event (Event a) -> Event a
keepLatest (Event e) =
  Event \k -> do
    cancelInner <- Ref.new Nothing
    cancelOuter <-
      e \inner -> do
        Ref.read cancelInner >>= sequence_
        c <- subscribe inner k
        Ref.write (Just c) cancelInner
    pure do
      Ref.read cancelInner >>= sequence_
      cancelOuter

-- | Compute a fixed point
fix :: forall i o. (Event i -> { input :: Event i, output :: Event o }) -> Event o
fix f =
  Event \k -> do
    c1 <- subscribe input push
    c2 <- subscribe output k
    pure (c1 *> c2)
  where
  { event, push } = unsafePerformEffect create

  { input, output } = f event

-- | Subscribe to an `Event` by providing a callback.
-- |
-- | `subscribe` returns a canceller function.
subscribe
  :: forall a
   . Event a
  -> (a -> Effect Unit)
  -> Effect (Effect Unit)
subscribe (Event e) k = e k

-- | Make an `Event` from a function which accepts a callback and returns an
-- | unsubscription function.
-- |
-- | Note: you probably want to use `create` instead, unless you need explicit
-- | control over unsubscription.
makeEvent
  :: forall a
   . ((a -> Effect Unit) -> Effect (Effect Unit))
  -> Event a
makeEvent = Event

type EventIO a =
  { event :: Event a
  , push :: a -> Effect Unit
  }

-- | Create an event and a function which supplies a value to that event.
create
  :: forall a
   . Effect (EventIO a)
create = do
  subscribers <- Ref.new []
  pure
    { event:
        Event \k -> do
          _ <- Ref.modify (_ <> [ k ]) subscribers
          pure do
            _ <- Ref.modify (deleteBy unsafeRefEq k) subscribers
            pure unit
    , push:
        \a -> do
          Ref.read subscribers >>= traverse_ \k -> k a
    }

bang :: forall a. a -> Event a
bang a =
  Event \k -> do
    k a
    pure (pure unit)

bus :: forall r a. ((a -> Effect Unit) -> Event a -> r) -> Event r
bus f = makeEvent \k -> do
  { push, event } <- create
  k (f push event)
  pure (pure unit)

memoize :: forall r a. Event a -> (Event a -> r) -> Event r
memoize e f = makeEvent \k -> do
  { push, event } <- create
  k (f event)
  subscribe e push