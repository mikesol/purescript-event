module FRP.Behavior
  ( Behavior
  , behavior
  , step
  , sample
  , sampleBy
  , sample_
  , gate
  , gateBy
  , unfold
  , switcher
  , integral
  , integral'
  , derivative
  , derivative'
  , solve
  , solve'
  , solve2
  , solve2'
  , fixB
  , animate
  ) where

import Prelude

import Control.Alt (alt)
import Control.Apply (lift2)
import Control.Monad.Error.Class (catchError)
import Data.Filterable (class Filterable, compact)
import Data.Function (applyFlipped)
import Data.HeytingAlgebra (ff, implies, tt)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Ref (new, read, write)
import Effect.Uncurried (mkEffectFn1, mkEffectFn2, runEffectFn1, runEffectFn2)
import FRP.Event (class IsEvent, Event, fix, fold, keepLatest, makeEvent, makeEventO, sampleOnRight, subscribe, subscribeO, withLast)
import FRP.Event.AnimationFrame (animationFrame)

-- | `ABehavio
-- | A `Behavior` acts like a continuous function of time.
-- |
-- | We can construct a sample a `Behavior` from some `Event`, combine `Behavior`s
-- | using `Applicative`, and sample a final `Behavior` on some other `Event`.
newtype Behavior a = Behavior (Effect (Tuple (Effect a) (Effect Unit)))

derive instance functorBehavior :: Functor Behavior

instance applyBehavior :: Apply Behavior where
  apply (Behavior efab) (Behavior ea) = Behavior (lift2 Tuple efab ea)

instance applicativeBehavior :: Applicative Behavior where
  pure a = Behavior \e -> applyFlipped a <$> e

instance semigroupBehavior :: Semigroup a => Semigroup (Behavior a) where
  append = lift2 append

instance monoidBehavior :: Monoid a => Monoid (Behavior a) where
  mempty = pure mempty

instance heytingAlgebraBehavior :: HeytingAlgebra a => HeytingAlgebra (Behavior a) where
  tt = pure tt
  ff = pure ff
  not = map not
  implies = lift2 implies
  conj = lift2 conj
  disj = lift2 disj

instance semiringBehavior :: Semiring a => Semiring (Behavior a) where
  zero = pure zero
  one = pure one
  add = lift2 add
  mul = lift2 mul

instance ringBehavior :: Ring a => Ring (Behavior a) where
  sub = lift2 sub

-- | Construct a `Behavior` from its sampling function.
behavior :: forall a. Effect (Tuple (Effect a) (Effect Unit)) -> Behavior a
behavior = Behavior

-- | Create a `Behavior` which is updated when an `Event` fires, by providing
-- | an initial value.
step :: forall a. a -> Event a -> Behavior a
step a e = Behavior do
  r <- new a
  u <- runEffectFn2 subscribeO e (mkEffectFn1 (flip write r))
  pure (Tuple (read r) u)

-- | Create a `Behavior` which is updated when an `Event` fires, by providing
-- | an initial value and a function to combine the current value with a new event
-- | to create a new value.
unfold :: forall a b. (b -> a -> b) -> b -> Event a -> Behavior b
unfold f a e = step a (fold f a e)

-- | Sample a `Behavior` on some `Event`.
sample :: forall a b. Behavior a -> Event (a -> b) -> Event b
sample (Behavior ea) eAb = makeEventO $ mkEffectFn1 \k -> do
  Tuple ba ua <- ea
  u <- runEffectFn2 subscribeO eAb $ mkEffectFn1 \ab -> do
    a <- ba
    runEffectFn1 k (ab a)
  pure do
    ua
    u

-- | Sample a `Behavior` on some `Event` by providing a combining function.
sampleBy :: forall a b c. (a -> b -> c) -> Behavior a -> Event b -> Event c
sampleBy f b e = sample (map f b) (map applyFlipped e)

-- | Sample a `Behavior` on some `Event`, discarding the event's values.
sample_ :: forall a b. Behavior a -> Event b -> Event a
sample_ = sampleBy const

-- | Switch `Behavior`s based on an `Event`.
switcher :: forall a. Behavior a -> Event (Behavior a) -> Behavior a
switcher b0 e = behavior \s ->
  keepLatest (pure (sample b0 s) `alt` map (\b -> sample b s) e)

-- | Sample a `Behavior` on some `Event` by providing a predicate function.
gateBy :: forall p a. (p -> a -> Boolean) -> Behavior p -> Event a -> Event a
gateBy f ps xs = compact (sampleBy (\p x -> if f p x then Just x else Nothing) ps xs)

-- | Filter an `Event` by the boolean value of a `Behavior`.
gate :: forall a. Behavior Boolean -> Event a -> Event a
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
  -> Behavior t
  -> Behavior a
  -> Behavior a
integral g initial t b =
  Behavior \e ->
    let
      x = sample b (e $> identity)
      y = withLast (sampleBy Tuple t x)
      z = fold approx initial y
    in
      sampleOnRight z e
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
  -> Behavior t
  -> Behavior t
  -> Behavior t
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
  -> Behavior t
  -> Behavior a
  -> Behavior a
derivative g t b =
  Behavior \e ->
    let
      x = sample b (e $> identity)
      y = withLast (sampleBy Tuple t x)
      z = map approx y
    in
      sampleOnRight z e
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
  => Behavior t
  -> Behavior t
  -> Behavior t
derivative' = derivative (_ $ identity)

-- | Compute a fixed point
fixB :: forall a. a -> (Behavior a -> Behavior a) -> Behavior a
fixB a f =
  behavior do
  r <- new a
  u <- runEffectFn2 subscribeO e (mkEffectFn1 (flip write r))
  pure (Tuple (read r) u)
    

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
  -> Behavior t
  -> (Behavior a -> Behavior a)
  -> Behavior a
solve g a0 t f = fixB a0 \b -> integral g a0 t (f b)

-- | Solve a first order differential equation.
-- |
-- | This function is a simpler version of `solve` where the function being
-- | integrated takes values in the same field used to represent time.
solve'
  :: forall a
   . Field a
  => a
  -> Behavior a
  -> (Behavior a -> Behavior a)
  -> Behavior a
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
  -> Behavior t
  -> (Behavior a -> Behavior a -> Behavior a)
  -> Behavior a
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
  -> Behavior a
  -> (Behavior a -> Behavior a -> Behavior a)
  -> Behavior a
solve2' = solve2 (_ $ identity)

-- | Animate a `Behavior` by providing a rendering function.
animate
  :: forall scene
   . Behavior Event scene
  -> (scene -> Effect Unit)
  -> Effect (Effect Unit)
animate scene render = subscribe (sample_ scene animationFrame) render
