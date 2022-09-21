module FRP.Event.Class
  ( class IsEvent
  , fold
  , folded
  , count
  , mapAccum
  , withLast
  , sampleOnRight
  , (<|**>)
  , sampleOnRightOp
  , (<|*>)
  , sampleOnRight_
  , (<|*)
  , sampleOnLeft
  , (<**|>)
  , sampleOnLeftOp
  , (<*|>)
  , sampleOnLeft_
  , (*|>)
  , applyOp
  , (<**>)
  , keepLatest
  , fix
  , gate
  , gateBy
  , module Data.Filterable
  ) where

import Prelude

import Control.Alternative (class Alternative, (<|>))
import Data.Compactable (compact)
import Data.Filterable (class Filterable, filterMap)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Tuple (Tuple(..), snd)

-- | Functions which an `Event` type should implement:
-- |
-- | - `fold`: combines incoming values using the specified function,
-- | starting with the specific initial value.
-- | - `keepLatest` flattens a nested event, reporting values only from the
-- | most recent inner event.
-- | - `sampleOn`: samples an event at the times when a second event fires.
-- | - `fix`: compute a fixed point, by feeding output events back in as
-- | inputs.
-- | - `bang`: A one-shot event that happens NOW.
class (Alternative event, Filterable event) <= IsEvent event where
  -- Temporal order for fold
  fold :: forall a b. (b -> a -> b) -> b -> event a -> event b
  keepLatest :: forall a. event (event a) -> event a
  sampleOnRight :: forall a b. event (a -> b) -> event a -> event b
  sampleOnLeft :: forall a b. event (a -> b) -> event a -> event b
  fix :: forall i o. (event i -> { input :: event i, output :: event o }) -> event o

infixl 4 sampleOnRight as <|**>
infixl 4 sampleOnLeft as <**|>

sampleOnRightOp :: forall event a b. IsEvent event => event a -> event (a -> b) -> event b
sampleOnRightOp ef ea = sampleOnRight ((#) <$> ef) ea

infixl 4 sampleOnRightOp as <|*>

sampleOnLeftOp :: forall event a b. IsEvent event => event (a -> b) -> event a -> event b
sampleOnLeftOp ef ea = sampleOnLeft (($) <$> ef) ea

infixl 4 sampleOnLeftOp as <*|>

applyOp :: forall event a b. Applicative event => event a -> event (a -> b) -> event b
applyOp ea ef = apply ((#) <$> ea) ef

infixl 4 applyOp as <**>

-- | Count the number of events received.
count :: forall event a. IsEvent event => event a -> event Int
count = fold (\n _ -> n + 1) 0

-- | Combine subsequent events using a `Monoid`.
folded :: forall event a. IsEvent event => Monoid a => event a -> event a
folded = fold append mempty

-- | Compute differences between successive event values.
withLast :: forall event a. IsEvent event => event a -> event { now :: a, last :: Maybe a }
withLast e = filterMap identity (fold step Nothing e)
  where
  step Nothing a = Just { now: a, last: Nothing }
  step (Just { now: b }) a = Just { now: a, last: Just b }

-- | Map over an event with an accumulator.
-- |
-- | For example, to keep the index of the current event:
-- |
-- | ```purescript
-- | mapAccum (\x i -> Tuple (i + 1) (Tuple x i)) 0`.
-- | ```
mapAccum :: forall event a b c. IsEvent event => (a -> b -> Tuple a c) -> a -> event b -> event c
mapAccum f acc xs = filterMap snd
  $ fold (\(Tuple a _) b -> pure <$> f a b) (Tuple acc Nothing) xs

-- | Create an `Event` which samples the latest values from the first event
-- | at the times when the second event fires, ignoring the values produced by
-- | the second event.
sampleOnRight_ :: forall event a b. IsEvent event => event a -> event b -> event a
sampleOnRight_ a b = sampleOnRight (const <$> a) b

infixl 4 sampleOnRight_ as <|*

sampleOnLeft_ :: forall event a b. IsEvent event => event a -> event b -> event b
sampleOnLeft_ a b = sampleOnLeft (const identity <$> a) b

infixl 4 sampleOnLeft_ as *|>

-- | Sample the events that are fired while a boolean event is true. Note that,
-- | until the boolean event fires, it will be assumed to be `false`, and events
-- | will be blocked.
gate :: forall a event. IsEvent event => event Boolean -> event a -> event a
gate = gateBy (\x _ -> fromMaybe false x)

-- | Generalised form of `gateBy`, allowing for any predicate between the two
-- | events. The predicate will not be evaluated until a value from the first event is received.
gateBy
  :: forall a b event
   . IsEvent event
  => (Maybe a -> b -> Boolean)
  -> event a
  -> event b
  -> event b
gateBy f sampled sampler = compact $
  (\p x -> if f p x then Just x else Nothing)
  <$> (pure Nothing <|> Just <$> sampled)
  <|**> sampler
