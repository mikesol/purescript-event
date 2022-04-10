module FRP.Event.Memoize
  ( memoize
  , isMemoized
  , isMemoizable
  , memoizeIfMemoizable
  , unsafeMemoize
  , class MemoizableEvent
  ) where

import Prelude

import Effect (Effect)
import FRP.Event as Event

unsafeMemoize :: Event.Event ~> Event.Event
unsafeMemoize = unsafeMemoizeImpl
foreign import unsafeMemoizeImpl :: Event.Event ~> Event.Event
-- | Memoize an event. **Memoized** events only initialize once.
-- | For example, if you create a
-- | memoized counter, it will start on the first subscription and subsequent
-- | subscriptions will subscribe to the _same_ counter.
foreign import memoizeImpl :: forall a. Event.Event a -> Effect (Event.Event a)
-- | Is an event memoized?
foreign import isMemoizedImpl :: forall a. Event.Event a -> Boolean
-- | Is an event memoizable?
foreign import isMemoizableImpl :: forall a. Event.Event a -> Boolean
-- | Memoize if memoizable
foreign import memoizeIfMemoizableImpl :: Event.Event ~> Event.Event

class MemoizableEvent :: forall k. (k -> Type) -> Constraint
class MemoizableEvent e where
  memoize :: forall a. e a -> Effect (e a)
  memoizeIfMemoizable :: e ~> e
  isMemoized :: forall a. e a -> Boolean
  isMemoizable :: forall a. e a -> Boolean

instance memoizableEvent :: MemoizableEvent Event.Event where
  memoize = memoizeImpl
  memoizeIfMemoizable = memoizeIfMemoizableImpl
  isMemoized = isMemoizedImpl
  isMemoizable = isMemoizableImpl
