module FRP.Poll
  ( APoll
  , Poll
  , sham
  , animate
  , poll
  , derivative
  , derivative'
  , effectToPoll
  , fixB
  , gate
  , gateBy
  , integral
  , integral'
  , refToPoll
  , sample
  , sampleBy
  , sample_
  , solve
  , solve'
  , solve2
  , solve2'
  , stRefToPoll
  , step
  , switcher
  , unfold
  ) where

import Prelude

import Control.Alt (class Alt, alt)
import Control.Apply (lift2)
import Control.Monad.ST.Class (liftST)
import Control.Monad.ST.Global (Global)
import Control.Monad.ST.Internal as STRef
import Control.Plus (class Plus, empty)
import Control.Semigroupoid (composeFlipped)
import Data.Compactable (separateDefault)
import Data.Either (Either, either)
import Data.Filterable as Filterable
import Data.Function (applyFlipped)
import Data.HeytingAlgebra (ff, implies, tt)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..), fst)
import Effect (Effect)
import Effect.Ref as Ref
import FRP.Event (class IsEvent, Event, fold, makeEvent, subscribe, withLast)
import FRP.Event.AnimationFrame (animationFrame)
import FRP.Event.Class as Event

-- | `APoll` is the more general type of `Poll`, which is parameterized
-- | over some underlying `event` type.
-- |
-- | Normally, you should use `Poll` instead, but this type
-- | can also be used with other types of events, including the ones in the
-- | `Semantic` module.
newtype APoll event a = APoll (forall b. event (a -> b) -> event b)

-- | A `Poll` acts like a survey or poll. We get a series of questions in time
-- | of form `a -> b`, and we respond with `b`.
-- |
-- | We can construct a sample a `Poll` from some `Event`, combine `Poll`s
-- | using `Applicative`, and sample a final `Poll` on some other `Event`.
type Poll = APoll Event

instance functorAPoll :: Functor event => Functor (APoll event) where
  map f (APoll b) = APoll \e -> b (map (_ <<< f) e)

instance applyAPoll :: Functor event => Apply (APoll event) where
  apply (APoll f) (APoll a) = APoll \e -> a (f (compose <$> e))

instance applicativeAPoll :: Functor event => Applicative (APoll event) where
  pure a = APoll \e -> applyFlipped a <$> e

instance semigroupAPoll :: (Functor event, Semigroup a) => Semigroup (APoll event a) where
  append = lift2 append

instance monoidAPoll :: (Functor event, Monoid a) => Monoid (APoll event a) where
  mempty = pure mempty

instance heytingAlgebraAPoll :: (Functor event, HeytingAlgebra a) => HeytingAlgebra (APoll event a) where
  tt = pure tt
  ff = pure ff
  not = map not
  implies = lift2 implies
  conj = lift2 conj
  disj = lift2 disj

instance semiringAPoll :: (Functor event, Semiring a) => Semiring (APoll event a) where
  zero = pure zero
  one = pure one
  add = lift2 add
  mul = lift2 mul

instance ringAPoll :: (Functor event, Ring a) => Ring (APoll event a) where
  sub = lift2 sub

-- | Construct a `Poll` from its sampling function.
poll :: forall event a. (forall b. event (a -> b) -> event b) -> APoll event a
poll = APoll

-- | Create a `Poll` which is updated when an `Event` fires, by providing
-- | an initial value.
step :: forall event a. IsEvent event => a -> event a -> APoll event a
step a e = APoll (\e0 -> Event.sampleOnRight ((Event.once e0 $> a) `alt` e) e0)

-- | Create a `Poll` which is updated when an `Event` fires, by providing
-- | an initial value and a function to combine the current value with a new event
-- | to create a new value.
unfold :: forall event a b. IsEvent event => (b -> a -> b) -> b -> event a -> APoll event b
unfold f a e = step a (fold f a e)

instance Alt event => Alt (APoll event) where
  alt (APoll a) (APoll b) = APoll \e -> a e `alt` b e

instance Plus event => Plus (APoll event) where
  empty = APoll \_ -> empty

-- | A poll where the answers are rigged by the nefarious `Event a`
sham :: forall event. IsEvent event => event ~> APoll event
sham i = poll \e -> Event.keepLatest (map (\f -> f <$> i) e)

-- | Sample a `Poll` on some `Event`.
sample :: forall event a b. APoll event a -> event (a -> b) -> event b
sample (APoll b) e = b e

-- | Sample a `Poll` on some `Event` by providing a combining function.
sampleBy :: forall event a b c. Functor event => (a -> b -> c) -> APoll event a -> event b -> event c
sampleBy f b e = sample (map f b) (map applyFlipped e)

-- | Sample a `Poll` on some `Event`, discarding the event's values.
sample_ :: forall event a b. Functor event => APoll event a -> event b -> event a
sample_ = sampleBy const

-- | Switch `Poll`s based on an `Event`.
switcher :: forall event a. IsEvent event => APoll event a -> event (APoll event a) -> APoll event a
switcher b0 e = poll \s ->
  Event.keepLatest ((Event.once s $> (sample b0 s)) `alt` map (\b -> sample b s) e)

-- | Sample a `Poll` on some `Event` by providing a predicate function.
gateBy :: forall event p a. Filterable.Filterable event => (p -> a -> Boolean) -> APoll event p -> event a -> event a
gateBy f ps xs = Filterable.compact (sampleBy (\p x -> if f p x then Just x else Nothing) ps xs)

-- | Filter an `Event` by the boolean value of a `Poll`.
gate :: forall event a. Filterable.Filterable event => APoll event Boolean -> event a -> event a
gate = gateBy const

-- | Integrate with respect to some measure of time.
-- |
-- | This function approximates the integral using the trapezium rule at the
-- | implicit sampling interval.
-- |
-- | The `Semiring` `a` should be a vector field over the field `t`. To represent
-- | this, the user should provide a _grate_ which lifts a multiplication
-- | function on `t` to a function on `a`. Simple examples where `t ~ a` can use
-- | the `integral'` function instead.
integral
  :: forall event a t
   . IsEvent event
  => Field t
  => Semiring a
  => (((a -> t) -> t) -> a)
  -> a
  -> APoll event t
  -> APoll event a
  -> APoll event a
integral g initial t b =
  APoll \e ->
    let
      x = sample b (e $> identity)
      y = withLast (sampleBy Tuple t x)
      z = fold approx initial y
    in
      Event.sampleOnRight z e
  where
  approx s { last: Nothing } = s
  approx s { now: Tuple t1 a1, last: Just (Tuple t0 a0) } = s + g (\f -> f (a0 + a1) * (t1 - t0) / two)

  two :: t
  two = one + one

-- | Integrate with respect to some measure of time.
-- |
-- | This function is a simpler version of `integral` where the function being
-- | integrated takes values in the same field used to represent time.
integral'
  :: forall event t
   . IsEvent event
  => Field t
  => t
  -> APoll event t
  -> APoll event t
  -> APoll event t
integral' = integral (_ $ identity)

-- | Differentiate with respect to some measure of time.
-- |
-- | This function approximates the derivative using a quotient of differences at the
-- | implicit sampling interval.
-- |
-- | The `Semiring` `a` should be a vector field over the field `t`. To represent
-- | this, the user should provide a grate which lifts a division
-- | function on `t` to a function on `a`. Simple examples where `t ~ a` can use
-- | the `derivative'` function.
derivative
  :: forall event a t
   . IsEvent event
  => Field t
  => Ring a
  => (((a -> t) -> t) -> a)
  -> APoll event t
  -> APoll event a
  -> APoll event a
derivative g t b =
  APoll \e ->
    let
      x = sample b (e $> identity)
      y = withLast (sampleBy Tuple t x)
      z = map approx y
    in
      Event.sampleOnRight z e
  where
  approx { last: Nothing } = zero
  approx { now: Tuple t1 a1, last: Just (Tuple t0 a0) } = g (\f -> f (a1 - a0) / (t1 - t0))

-- | Differentiate with respect to some measure of time.
-- |
-- | This function is a simpler version of `derivative` where the function being
-- | differentiated takes values in the same field used to represent time.
derivative'
  :: forall event t
   . IsEvent event
  => Field t
  => APoll event t
  -> APoll event t
  -> APoll event t
derivative' = derivative (_ $ identity)

-- | Compute a fixed point
fixB :: forall event a. IsEvent event => a -> (APoll event a -> APoll event a) -> APoll event a
fixB a f =
  poll \s ->
    Event.sampleOnRight
      ( Event.fix \event ->
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
-- | This function is a simpler version of `solve` where the function being
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
-- | This function is a simpler version of `solve2` where the function being
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
   . APoll Event scene
  -> (scene -> Effect Unit)
  -> Effect (Effect Unit)
animate scene render = do
  { event, unsubscribe } <- animationFrame
  u2 <- liftST $ subscribe (sample_ scene event) render
  pure do
    unsubscribe
    liftST u2

-- | Turn an ST Ref into a poll
stRefToPoll :: STRef.STRef Global ~> Poll
stRefToPoll r = do
  poll \e -> makeEvent \k -> subscribe e \f -> liftST (STRef.read r) >>= k <<< f

-- | Turn a Ref into a poll
refToPoll :: Ref.Ref ~> Poll
refToPoll r = do
  poll \e -> makeEvent \k -> subscribe e \f -> Ref.read r >>= k <<< f

-- | Turn an Effect into a poll
effectToPoll :: Effect ~> Poll
effectToPoll ee = do
  poll \e -> makeEvent \k -> subscribe e \f -> ee >>= k <<< f

filterMap
  :: forall event a b
   . Filterable.Compactable event
  => Functor event
  => (a -> Maybe b)
  -> APoll event a
  -> APoll event b
filterMap f b = poll \e -> Filterable.compact
  $ sampleBy (\a ff -> map ff $ f a) b e

compact :: forall event a. Filterable.Compactable event => Functor event => APoll event (Maybe a) -> APoll event a
compact = filterMap identity

partitionMap :: forall event a b c. Filterable.Compactable event => Functor event => (a -> Either b c) -> APoll event a -> { left :: APoll event b, right :: APoll event c }
partitionMap f b = { left: filterMap (either Just (const Nothing)) fb, right: filterMap (either (const Nothing) Just) fb }
  where
  fb = f <$> b

instance (Functor event, Filterable.Compactable event) => Filterable.Compactable (APoll event) where
  compact = compact
  separate = separateDefault

instance (Functor event, Filterable.Compactable event) => Filterable.Filterable (APoll event) where
  filterMap = filterMap
  filter = Filterable.filterDefault
  partitionMap = partitionMap
  partition = Filterable.partitionDefault

sampleOnRight :: forall event a b. IsEvent event => APoll event a -> APoll event (a -> b) -> APoll event b
sampleOnRight a b = poll \e -> Event.sampleOnRight (sample_ a e) (sampleBy composeFlipped b e)

sampleOnLeft :: forall event a b. IsEvent event => APoll event a -> APoll event (a -> b) -> APoll event b
sampleOnLeft a b = poll \e -> Event.sampleOnLeft (sample_ a e) (sampleBy composeFlipped b e)

keepLatest :: forall event a. IsEvent event => APoll event (APoll event a) -> APoll event a
keepLatest a = poll \e -> Event.keepLatest (sample_ (map (flip sample e) a) e)

fix :: forall event a. IsEvent event => (APoll event a -> APoll event a) -> APoll event a
fix f = poll \e -> (\(Tuple a ff) -> ff a) <$> Event.fix \ee -> sampleBy Tuple (f (sham (fst <$> ee))) e

once :: forall event a. IsEvent event => APoll event a -> APoll event a
once a = poll \e -> Event.once (sample a e)

instance (IsEvent event, Plus event) => IsEvent (APoll event) where
  sampleOnRight = sampleOnRight
  sampleOnLeft = sampleOnLeft
  keepLatest = keepLatest
  fix = fix
  once = once