module FRP.Poll
  ( Poll(..)
  , PollIO
  , class Pollable
  , sample
  , sample_
  , sampleBy
  , PurePollIO
  , animate
  , create
  , createPure
  , derivative
  , derivative'
  , dredge
  , fixB
  , gate
  , gateBy
  , integral
  , integral'
  , mailbox
  , merge
  , mergeMap
  , poll
  , pollFromEvent
  , pollFromOptimizedRep
  , pollFromPoll
  , rant
  , deflect
  , sham
  , solve
  , solve'
  , solve2
  , solve2'
  , stRefToPoll
  , stToPoll
  , step
  , switcher
  , toPoll
  , unfold
  ) where

import Prelude

import Control.Alt (class Alt, alt, (<|>))
import Control.Apply (lift2)
import Control.Monad.ST.Global (Global)
import Control.Monad.ST.Internal (ST)
import Control.Monad.ST.Internal as STRef
import Control.Plus (class Plus, empty)
import Data.Array as Array
import Data.Either (Either, either)
import Data.Filterable (eitherBool, maybeBool)
import Data.Filterable as Filterable
import Data.Foldable (foldr, oneOfMap)
import Data.Function (applyFlipped)
import Data.FunctorWithIndex (class FunctorWithIndex, mapWithIndex)
import Data.HeytingAlgebra (ff, implies, tt)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Profunctor (dimap)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import FRP.Event (class IsEvent, Event, fold, justMany, makeEvent, subscribe)
import FRP.Event as Event
import FRP.Event.AnimationFrame (animationFrame)
import FRP.Event.Class as EClass
import FRP.Poll.Unoptimized as Poll
import Unsafe.Coerce (unsafeCoerce)

-- | `Poll` is an optimized version of poll fine-tuned for `Event`.
data Poll a
  = OnlyPure (Array a)
  | OnlyPoll (Poll.APoll Event a)
  | OnlyEvent (Event a)
  | PureAndPoll (Array a) (Poll.APoll Event a)


instance functorAPoll :: Functor Poll where
  map f (PureAndPoll x y) = PureAndPoll (map f x) (map f y)
  map f (OnlyPure x) = OnlyPure (map f x)
  map f (OnlyEvent x) = OnlyEvent (map f x)
  map f (OnlyPoll y) = OnlyPoll (map f y)

instance functorWithIndexAPoll :: FunctorWithIndex Int Poll where
  mapWithIndex f (PureAndPoll x y) = PureAndPoll (mapWithIndex f x) (EClass.mapAccum (\a b -> Tuple (a + 1) (f a b)) (Array.length x) y)
  mapWithIndex f (OnlyPure x) = OnlyPure (mapWithIndex f x)
  mapWithIndex f (OnlyEvent x) = OnlyEvent (mapWithIndex f x)
  mapWithIndex f (OnlyPoll y) = OnlyPoll (EClass.mapAccum (\a b -> Tuple (a + 1) (f a b)) (0) y)

instance applyAPoll :: Apply Poll where
  apply (OnlyEvent a) (OnlyEvent b) = OnlyEvent (a <*> b)
  apply a b = pollFromPoll (toPoll a <*> toPoll b)

instance applicativeAPoll :: Applicative Poll where
  pure a = OnlyPure [ a ]

instance semigroupAPoll :: Semigroup a => Semigroup (Poll a) where
  append = lift2 append

instance monoidAPoll :: Monoid a => Monoid (Poll a) where
  mempty = pure mempty

instance heytingAlgebraAPoll :: HeytingAlgebra a => HeytingAlgebra (Poll a) where
  tt = pure tt
  ff = pure ff
  not = map not
  implies = lift2 implies
  conj = lift2 conj
  disj = lift2 disj

instance semiringAPoll :: Semiring a => Semiring (Poll a) where
  zero = pure zero
  one = pure one
  add = lift2 add
  mul = lift2 mul

instance ringAPoll :: Ring a => Ring (Poll a) where
  sub = lift2 sub

pollFromPoll :: Poll.Poll ~> Poll
pollFromPoll i = OnlyPoll i

-- | Construct a `Poll` from its sampling function.
pollFromEvent :: Event ~> Poll
pollFromEvent = OnlyEvent

pollFromOptimizedRep :: forall a. Array a -> Event a -> Poll a
pollFromOptimizedRep a i = PureAndPoll a (Poll.sham i)

poll :: forall a. (forall b. Event (a -> b) -> Event b) -> Poll a
poll f = OnlyPoll (Poll.poll f)

toPoll :: Poll ~> Poll.Poll
toPoll (PureAndPoll a b) = oneOfMap pure a <|> b
toPoll (OnlyEvent a) = Poll.sham a
toPoll (OnlyPure a) = oneOfMap pure a
toPoll (OnlyPoll b) = b

-- | Create a `Poll` which is updated when an `Event` fires, by providing
-- | an initial value.
step :: forall a. a -> Event a -> Poll a
step a e = PureAndPoll [ a ] $ Poll.poll \e0 -> EClass.sampleOnRight e e0

-- | Create a `Poll` which is updated when an `Event` fires, by providing
-- | an initial value and a function to combine the current value with a new event
-- | to create a new value.
unfold :: forall a b. (b -> a -> b) -> b -> Event a -> Poll b
unfold f a e = step a (fold f a e)

instance Alt Poll where
  alt (OnlyPure a) (OnlyPure x) = OnlyPure (a <> x)
  alt (OnlyPure a) (OnlyEvent y) = PureAndPoll a (Poll.sham y)
  alt (OnlyPure a) (OnlyPoll y) = PureAndPoll a y
  alt (OnlyPure a) (PureAndPoll x y) = PureAndPoll (a <> x) y
  alt (OnlyEvent b) (OnlyPure x) = PureAndPoll x (Poll.sham b)
  alt (OnlyEvent b) (OnlyEvent y) = OnlyEvent (alt b y)
  alt (OnlyEvent b) (OnlyPoll y) = OnlyPoll (alt (Poll.sham b) y)
  alt (OnlyEvent b) (PureAndPoll x y) = PureAndPoll x (alt (Poll.sham b) y)
  alt (OnlyPoll b) (OnlyPure x) = PureAndPoll x b
  alt (OnlyPoll b) (OnlyEvent y) = OnlyPoll (alt b $ Poll.sham y)
  alt (OnlyPoll b) (OnlyPoll y) = OnlyPoll (alt b y)
  alt (OnlyPoll b) (PureAndPoll x y) = PureAndPoll x (alt b y)
  alt (PureAndPoll a b) (OnlyPure x) = PureAndPoll (a <> x) b
  alt (PureAndPoll a b) (OnlyEvent y) = PureAndPoll a (alt b $ Poll.sham y)
  alt (PureAndPoll a b) (OnlyPoll y) = PureAndPoll a (alt b y)
  alt (PureAndPoll a b) (PureAndPoll x y) = PureAndPoll (a <> x) (alt b y)

instance Plus Poll where
  empty = OnlyPure []

-- | Merge together several polls. OnlyPure has the same functionality
-- | as `oneOf`, but it is faster and less prone to stack explosions.
merge :: forall a. Array (Poll a) → Poll a
merge a = case foldr go { l: [], m: [], r: [] } a of
  { l, m: [], r: [] } -> OnlyPure l
  { l:[], m, r: [] } -> OnlyEvent (Event.merge m)
  { l: [], m: [], r } -> OnlyPoll (Poll.merge r)
  -- todo: is it problematic that this is out of l2r order?
  { l, m, r } -> PureAndPoll l (Poll.sham (Event.merge m) <|> Poll.merge r)
  where

  go (OnlyPure q) { l, m, r } = { l: l <> q, m, r }
  go (OnlyEvent q) { l, m, r } = { l, m: m <> [ q ], r }
  go (OnlyPoll q) { l, m, r } = { l, m, r: r <> [ q ] }
  go (PureAndPoll x y) { l, m, r } = { l: l <> x, m, r: r <> [ y ] }

-- mergeMap is perfunctory here
mergeMap :: forall a b. (a -> Poll b) -> Array a → Poll b
mergeMap f a = merge (map f a)

-- | A poll where the answers are rigged by the nefarious `Event a`
sham :: Event ~> Poll
sham = pollFromEvent

-- | Turn a function over events into a function over polls.
dredge :: forall a b.  (Event a -> Event b) -> Poll a -> Poll b
dredge f ea = pollFromPoll (Poll.dredge f (toPoll ea))

-- | Switch `Poll`s based on an `Event`.
switcher :: forall a. Poll a -> Event (Poll a) -> Poll a
switcher b e = EClass.keepLatest (pollFromOptimizedRep [ b ] e)

-- | Sample a `Poll` on some `Event` by providing a predicate function.
gateBy :: forall p a. (p -> a -> Boolean) -> Poll p -> Event a -> Event a
gateBy f ps xs = Filterable.compact (sampleBy (\p x -> if f p x then Just x else Nothing) ps xs)

-- | Filter an `Event` by the boolean value of a `Poll`.
gate :: forall a.Poll Boolean -> Event a -> Event a
gate = gateBy const

-- | Integrate with respect to some measure of time.
-- |
-- | OnlyPure function approximates the integral using the trapezium rule at the
-- | implicit sampling interval.
-- |
-- | The `Semiring` `a` should be a vector field over the field `t`. To represent
-- | this, the user should provide a _grate_ which lifts a multiplication
-- | function on `t` to a function on `a`. Simple examples where `t ~ a` can use
-- | the `integral'` function instead.
integral
  :: forall a t
   . Field t
  => Semiring a
  => (((a -> t) -> t) -> a)
  -> a
  -> Poll t
  -> Poll a
  -> Poll a
integral g initial t b = pollFromPoll $ Poll.integral g initial (toPoll t) (toPoll b)

-- | Integrate with respect to some measure of time.
-- |
-- | OnlyPure function is a simpler version of `integral` where the function being
-- | integrated takes values in the same field used to represent time.
integral'
  :: forall t
   . Field t
  => t
  -> Poll t
  -> Poll t
  -> Poll t
integral' = integral (_ $ identity)

-- | Differentiate with respect to some measure of time.
-- |
-- | OnlyPure function approximates the derivative using a quotient of differences at the
-- | implicit sampling interval.
-- |
-- | The `Semiring` `a` should be a vector field over the field `t`. To represent
-- | this, the user should provide a grate which lifts a division
-- | function on `t` to a function on `a`. Simple examples where `t ~ a` can use
-- | the `derivative'` function.
derivative
  :: forall a t
   . Field t
  => Ring a
  => (((a -> t) -> t) -> a)
  -> Poll t
  -> Poll a
  -> Poll a
derivative g t b = pollFromPoll $ Poll.derivative g (toPoll t) (toPoll b)

-- | Differentiate with respect to some measure of time.
-- |
-- | OnlyPure function is a simpler version of `derivative` where the function being
-- | differentiated takes values in the same field used to represent time.
derivative'
  :: forall t
   . Field t
  => Poll t
  -> Poll t
  -> Poll t
derivative' = derivative (_ $ identity)

-- | Compute a fixed point
fixB :: forall a. a -> (Poll a -> Poll a) -> Poll a
fixB a f =
  poll \s ->
    EClass.sampleOnRight
      ( EClass.fix \event ->
          let
            b = f (step a event)
          in
            sample_ b s
      )
      s

-- | Solve a first order differential equation of the form
-- |
-- | ```
-- | da/dt = f a
-- | ```
-- |
-- | by integrating once (specifying the initial conditions).
-- |
-- | For example, the exponential function with growth rate `⍺`:
-- |
-- | ```purescript
-- | exp = solve' 1.0 Time.seconds (⍺ * _)
-- | ```
solve
  :: forall t a
   . Field t
  => Semiring a
  => (((a -> t) -> t) -> a)
  -> a
  -> Poll t
  -> (Poll a -> Poll a)
  -> Poll a
solve g a0 t f = fixB a0 \b -> integral g a0 t (f b)

-- | Solve a first order differential equation.
-- |
-- | OnlyPure function is a simpler version of `solve` where the function being
-- | integrated takes values in the same field used to represent time.
solve'
  :: forall a
   . Field a
  => a
  -> Poll a
  -> (Poll a -> Poll a)
  -> Poll a
solve' = solve (_ $ identity)

-- | Solve a second order differential equation of the form
-- |
-- | ```
-- | d^2a/dt^2 = f a (da/dt)
-- | ```
-- |
-- | by integrating twice (specifying the initial conditions).
-- |
-- | For example, an (damped) oscillator:
-- |
-- | ```purescript
-- | oscillate = solve2' 1.0 0.0 Time.seconds (\x dx -> -⍺ * x - δ * dx)
-- | ```
solve2
  :: forall t a
   . Field t
  => Semiring a
  => (((a -> t) -> t) -> a)
  -> a
  -> a
  -> Poll t
  -> (Poll a -> Poll a -> Poll a)
  -> Poll a
solve2 g a0 da0 t f =
  fixB a0 \b ->
    integral g a0 t
      ( fixB da0 \db ->
          integral g da0 t (f b db)
      )

-- | Solve a second order differential equation.
-- |
-- | OnlyPure function is a simpler version of `solve2` where the function being
-- | integrated takes values in the same field used to represent time.
solve2'
  :: forall a
   . Field a
  => a
  -> a
  -> Poll a
  -> (Poll a -> Poll a -> Poll a)
  -> Poll a
solve2' = solve2 (_ $ identity)

-- | Animate a `Poll` by providing a rendering function.
animate
  :: forall scene
   . Poll scene
  -> (scene -> Effect Unit)
  -> Effect (Effect Unit)
animate scene render = do
  { event, unsubscribe } <- animationFrame
  u2 <- subscribe (sample_ scene event) render
  pure do
    unsubscribe
    u2

-- | Turn an ST Ref into a poll
stRefToPoll :: STRef.STRef Global ~> Poll
stRefToPoll r = pollFromPoll (Poll.stRefToPoll r)

-- | Turn an ST Global into a poll
stToPoll :: ST Global ~> Poll
stToPoll r = pollFromPoll (Poll.stToPoll r)

filterMap
  :: forall a b
   . (a -> Maybe b)
  -> Poll a
  -> Poll b
filterMap f (PureAndPoll x y) = PureAndPoll (Filterable.filterMap f x) (Filterable.filterMap f y)
filterMap f (OnlyPure x) = OnlyPure (Filterable.filterMap f x)
filterMap f (OnlyEvent x) = OnlyEvent (Filterable.filterMap f x)
filterMap f (OnlyPoll y) = OnlyPoll (Filterable.filterMap f y)

partitionMap :: forall a b c. (a -> Either b c) -> Poll a -> { left :: Poll b, right :: Poll c }
partitionMap f b = { left: filterMap (either Just (const Nothing)) fb, right: filterMap (either (const Nothing) Just) fb }
  where
  fb = f <$> b

instance Filterable.Compactable Poll where
  compact = filterMap identity
  separate = partitionMap identity

instance Filterable.Filterable Poll where
  filterMap = filterMap
  filter = filterMap <<< maybeBool
  partitionMap = partitionMap
  partition p xs = do
    let o = partitionMap (eitherBool p) xs
    { no: o.left, yes: o.right }

sampleOnRight
  :: forall a b
   . Poll a
  -> Poll (a -> b)
  -> Poll b
sampleOnRight (OnlyEvent a) (OnlyEvent b) = OnlyEvent (a `EClass.sampleOnRight` b)
sampleOnRight a b = pollFromPoll (toPoll a `EClass.sampleOnRight` toPoll b)

sampleOnLeft :: forall a b.  Poll a -> Poll (a -> b) -> Poll b
sampleOnLeft (OnlyEvent a) (OnlyEvent b) = OnlyEvent (a `EClass.sampleOnLeft` b)
sampleOnLeft a b = pollFromPoll (toPoll a `EClass.sampleOnLeft` toPoll b)

fix
  :: forall a
   . (Poll a -> Poll a)
  -> Poll a
fix f = pollFromPoll $ EClass.fix (dimap pollFromPoll toPoll f)

once :: Poll ~> Poll
once i = pollFromPoll $ EClass.once (toPoll i)

instance IsEvent Poll where
  sampleOnRight = sampleOnRight
  sampleOnLeft = sampleOnLeft
  keepLatest = keepLatest
  fix = fix
  once = once

type PollIO a = { poll :: Poll a, push :: a -> Effect Unit }
type PurePollIO a = { poll :: Poll a, push :: a -> ST Global Unit }

create
  :: forall a
   . ST Global (PollIO a)
create = do
  { event, push } <- Event.create
  { poll: p } <- rant (sham event)
  pure { poll: p, push }

createPure
  :: forall a
   . ST Global (PurePollIO a)
createPure = do
  { event, push } <- Event.createPure
  pure { poll: sham event, push }

mailbox
  :: forall a b
   . Ord a
  => ST Global { push :: { address :: a, payload :: b } -> Effect Unit, poll :: a -> Poll b }
mailbox = do
  { push, event } <- Event.mailbox
  pure { poll: map sham event, push }

-- Rant never emits the head, so we can just ignore it
rant
  :: forall a
   . Poll a
  -> ST Global { poll :: Poll a, unsubscribe :: ST Global Unit }
rant (PureAndPoll _ i) = do
  { poll: p, unsubscribe } <- Poll.rant i
  pure $ { poll: OnlyPoll p, unsubscribe }
-- todo: we should add or remove effect from some of these types
rant (OnlyEvent i) =
  ( unsafeCoerce
      :: Effect
           { poll :: Poll a
           , unsubscribe :: Effect Unit
           }
      -> ST Global
           { poll :: Poll a
           , unsubscribe :: ST Global Unit
           }
  ) do
    { event: e, unsubscribe } <- Event.memoize i
    pure $ { poll: OnlyEvent e, unsubscribe }
rant (OnlyPoll i) = do
  { poll: p, unsubscribe } <- Poll.rant i
  pure $ { poll: OnlyPoll p, unsubscribe }
rant (OnlyPure _) = pure $ { poll: empty, unsubscribe: pure unit }

deflect
  :: forall a
   . Poll a
  -> ST Global (Poll a)
deflect (PureAndPoll a b) = PureAndPoll a <$> Poll.deflect b
deflect (OnlyPoll b) = OnlyPoll <$> Poll.deflect b
deflect (OnlyEvent _) = pure $ OnlyPure []
deflect (OnlyPure a) = pure (OnlyPure a)

keepLatest
  :: forall a
   . Poll (Poll a)
  -> Poll a
keepLatest (OnlyPure p) = fromMaybe empty (Array.last p)
keepLatest p = pollFromPoll $ EClass.keepLatest (toPoll (map toPoll p))


class Pollable pollable where
  -- | Sample a `Poll` on some `Event`.
  sample :: forall a b. Poll a -> pollable (a -> b) -> pollable b

instance Pollable Event where
  sample (PureAndPoll x y) ab = e <|> Poll.sample y ab
    where
    e = makeEvent \s -> s ab \f -> justMany (map f x)
  sample (OnlyEvent y) ab = y `EClass.sampleOnLeft` ab
  sample (OnlyPoll y) ab = Poll.sample y ab
  sample (OnlyPure x) ab = e
    where
    e = makeEvent \s -> s ab \f -> justMany (map f x)
instance Pollable Poll where
  sample = EClass.sampleOnRight

-- | Sample a `Poll` on some `Event` by providing a combining function.
sampleBy :: forall pollable a b c. Pollable pollable => Functor pollable => (a -> b -> c) -> Poll a -> pollable b -> pollable c
sampleBy f b e = sample (map f b) (map applyFlipped e)

-- | Sample a `Poll` on some `Event`, discarding the event's values.
sample_ :: forall pollable a b. Pollable  pollable =>  Functor pollable => Poll a -> pollable b -> pollable a
sample_ = sampleBy const