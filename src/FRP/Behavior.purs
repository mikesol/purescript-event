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

import Control.Apply (lift2)
import Control.Monad.Error.Class (catchError)
import Control.Monad.ST (ST)
import Control.Monad.ST.Class (liftST)
import Control.Monad.ST.Global (Global)
import Control.Monad.ST.Ref (STRef, modify, new, read, write)
import Data.Filterable (compact)
import Data.Function (applyFlipped)
import Data.HeytingAlgebra (ff, implies, tt)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..), fst)
import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Effect.Class (liftEffect)
import Effect.Exception (Error)
import Effect.Uncurried (mkEffectFn1, runEffectFn1, runEffectFn2)
import FRP.Event (Event, fold, makeEventO, subscribe, subscribeO)
import FRP.Event.AnimationFrame (animationFrame)

-- | `ABehavio
-- | A `Behavior` acts like a continuous function of time.
-- |
-- | We can construct a sample a `Behavior` from some `Event`, combine `Behavior`s
-- | using `Applicative`, and sample a final `Behavior` on some other `Event`.
data Behavior a = Behavior (Error -> ST Global a) (Effect (Tuple (Effect Unit) (Aff a)))

derive instance functorBehavior :: Functor Behavior

instance applyBehavior :: Apply Behavior where
  apply (Behavior errfab efab) (Behavior erra ea) = Behavior (lift2 apply errfab erra) do
    Tuple ua a <- ea
    Tuple ub ab <- efab
    pure $ Tuple (ub *> ua) (ab <*> a)

instance applicativeBehavior :: Applicative Behavior where
  pure a = Behavior (const $ pure a) (pure (Tuple (pure unit) (pure a)))

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
behavior :: forall a. (Error -> ST Global a) -> Effect (Tuple (Effect Unit) (Aff a)) -> Behavior a
behavior = Behavior

write_ :: forall region a. a -> STRef region a -> ST region Unit
write_ a b = void $ write a b

-- | Create a `Behavior` which is updated when an `Event` fires, by providing
-- | an initial value.
step :: forall a. a -> Event a -> Behavior a
step a e = Behavior (const $ pure a) $ do
  r <- liftST $ new a
  u <- runEffectFn2 subscribeO e (mkEffectFn1 (liftST <<< flip write_ r))
  pure (Tuple u (liftEffect $ liftST $ read r))

-- | Create a `Behavior` which is updated when an `Event` fires, by providing
-- | an initial value and a function to combine the current value with a new event
-- | to create a new value.
unfold :: forall a b. (b -> a -> b) -> b -> Event a -> Behavior b
unfold f a e = step a (fold f a e)

-- | Sample a `Behavior` on some `Event`.
sample :: forall a b. Behavior a -> Event (a -> b) -> Event b
sample (Behavior erra ea) eAb = makeEventO $ mkEffectFn1 \k -> do
  Tuple ua ba <- ea
  u <- runEffectFn2 subscribeO eAb $ mkEffectFn1 \ab -> launchAff_ do
    a <- catchError ba ((liftEffect <<< liftST) <$> erra)
    liftEffect $ runEffectFn1 k (ab a)
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
switcher (Behavior ef b0) e = behavior ef do
  r <- b0 >>= liftST <<< new <<< Tuple ef
  u <- runEffectFn2 subscribeO e $ mkEffectFn1 \(Behavior eef b) -> do
    Tuple _ (Tuple x _) <- liftST $ read r
    x
    b >>= liftST <<< flip write_ r <<< Tuple eef
  let
    l = do
      Tuple _ (Tuple x _) <- liftST $ read r
      x
      u
  pure $ Tuple l do
    Tuple eef (Tuple _ x) <- liftST $ read r
    catchError x (liftST <$> eef)

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
  :: forall a t
   . Field t
  => Semiring a
  => (((a -> t) -> t) -> a)
  -> a
  -> Behavior t
  -> Behavior a
  -> Behavior a
integral g initial t b = map fst $ fixB (Tuple initial Nothing)
  ( \prev ->
      ( \(Tuple s x) (Tuple t1 a1) -> case x of
          Nothing -> Tuple s (Just (Tuple t1 a1))
          Just (Tuple t0 a0) -> Tuple (s + g (\f -> f (a0 + a1) * (t1 - t0) / two)) (Just (Tuple t1 a1))
      ) <$> prev <*> (Tuple <$> t <*> b)
  )
  -- Behavior \e ->
  --   let
  --     x = sample b (e $> identity)
  --     y = withLast (sampleBy Tuple t x)
  --     z = fold approx initial y
  --   in
  --     sampleOnRight z e
  -- where
  -- approx s { last: Nothing } = s
  -- approx s { now: Tuple t1 a1, last: Just (Tuple t0 a0) } = s + g (\f -> f (a0 + a1) * (t1 - t0) / two)
  where
  two :: t
  two = one + one

-- | Integrate with respect to some measure of time.
-- |
-- | This function is a simpler version of `integral` where the function being
-- | integrated takes values in the same field used to represent time.
integral'
  :: forall t
   . Field t
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
  :: forall a t
   . Field t
  => Ring a
  => (((a -> t) -> t) -> a)
  -> Behavior t
  -> Behavior a
  -> Behavior a
derivative g t b = map fst $ fixB (Tuple zero Nothing)
  ( \prev ->
      ( \(Tuple _ x) (Tuple t1 a1) -> case x of
          Nothing -> Tuple zero (Just (Tuple t1 a1))
          Just (Tuple t0 a0) -> Tuple (g (\f -> f (a1 - a0) / (t1 - t0)))
            ( Just (Tuple t1 a1)
            )
      ) <$> prev <*> (Tuple <$> t <*> b)
  )

-- | Differentiate with respect to some measure of time.
-- |
-- | This function is a simpler version of `derivative` where the function being
-- | differentiated takes values in the same field used to represent time.
derivative'
  :: forall t
   . Field t
  => Behavior t
  -> Behavior t
  -> Behavior t
derivative' = derivative (_ $ identity)

-- | Compute a fixed point
fixB :: forall a. a -> (Behavior a -> Behavior a) -> Behavior a
fixB a f = Behavior (const $ pure a) do
  b <- liftST $ new a
  let
    -- todo, we need to use this more intelligently going forward
    Behavior ef o = f $ Behavior (const $ read b) $ pure do
      Tuple (pure unit) (liftST $ read b)
  Tuple u x <- o
  pure $ Tuple u do
    catchError x (liftST <$> ef) >>= liftST <<< flip modify b <<< const

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
   . Behavior scene
  -> (scene -> Effect Unit)
  -> Effect (Effect Unit)
animate scene render = subscribe (sample_ scene animationFrame) render
