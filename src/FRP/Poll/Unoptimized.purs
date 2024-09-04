module FRP.Poll.Unoptimized
  ( APoll
  , Poll
  , animate
  , class Pollable
  , create
  , createPure
  , createTagged
  , deflect
  , derivative
  , derivative'
  , dredge
  , fixWithInitial
  , gate
  , gateBy
  , integral
  , integral'
  , listen_
  , mailbox
  , mailboxS
  , merge
  , mergeMap
  , poll
  , rant
  , sample
  , sampleBy
  , sample_
  , sham
  , solve
  , solve'
  , solve2
  , solve2'
  , stRefToPoll
  , stToPoll
  , step
  , switcher
  , unfold
  ) where

import Prelude

import Control.Alt (class Alt, alt)
import Control.Apply (lift2)
import Control.Monad.ST.Class (liftST)
import Control.Monad.ST.Global (Global)
import Control.Monad.ST.Internal (ST)
import Control.Monad.ST.Internal as STRef
import Control.Plus (class Plus, empty)
import Control.Semigroupoid (composeFlipped)
import Data.Array as Array
import Data.Either (Either, either)
import Data.Filterable (eitherBool, maybeBool)
import Data.Filterable as Filterable
import Data.Foldable (oneOf)
import Data.Function (applyFlipped)
import Data.FunctorWithIndex (class FunctorWithIndex)
import Data.HeytingAlgebra (ff, implies, tt)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..), fst)
import Effect (Effect)
import FRP.Event (class IsEvent, Event, EventfulProgram, ProgramfulEvent, fold, justManyM, justNone, justOne, justOneM, makeEvent, subscribe, withLast)
import FRP.Event as Event
import FRP.Event.AnimationFrame (animationFrame)
import FRP.Event.Class as EClass

-- | `APoll` is the more general type of `Poll`, which is parameterized
-- | over some underlying `event` type.
-- |
-- | Normally, you should use `Poll` instead, but this type
-- | can also be used with other types of events, including the ones in the
-- | `Semantic` module.
newtype APoll event a = APoll (forall r. event (a -> r) -> event r)

-- | A `Poll` acts like a survey or poll. We get a series of questions in time
-- | of form `a -> r`, and we respond with `r`.
-- |
-- | We can construct a `Poll` from some `Event`, combine `Poll`s
-- | using `Applicative`, and sample a final `Poll` on some other `Event`.
-- |
-- | Very similar to Yoneda `newtype Yoneda f a = Yoneda (forall b. (a -> r) -> f r)`.
-- | Yoneda lemma tells that `(a -> r) -> r` is same as having just `a`
-- | and `(a -> r) -> f r` is same as having just `f a`.
type Poll = APoll Event

instance functorAPoll :: Functor event => Functor (APoll event) where
  map f (APoll b) = APoll \e -> b (map (_ <<< f) e)

instance functorWithIndexAPoll :: (IsEvent event, Pollable event event) => FunctorWithIndex Int (APoll event) where
  mapWithIndex f e = EClass.mapAccum (\a b -> Tuple (a + 1) (f a b)) 0 e

instance applyAPoll :: Apply event => Apply (APoll event) where
  apply (APoll f) (APoll a) = APoll \e -> (map (\ff (Tuple bc aaa) -> bc (ff aaa)) (f (e $> identity))) <*> a (map Tuple e)

-- Intuitively works like a highly dense infinite stream
-- `List (Cow, Time) = [ (C1,0), (C1,1), ... ]`
--
-- will repeat for each push
--
-- ```purescript
-- data Cow = C1 | C2
-- test = do
--   surveymonger <- liftST $ createPure -- List (Cow -> MichlenStars, Time)
--   let
--     (whoWantsToAnswer :: Poll Cow) = pure C1 <|> pure C2 -- will give to callback both immediately, on ofter another
--     (answers :: Event Int) = sample whoWantsToAnswer surveymonger.event
--   _ <- subscribe answers \v ->  log $ "answers GOT " <> show v
--   surveymonger.push(...)
-- ```
--
-- push to `surveymonger` | C1->1       C1->11
--                        | C2->2       C2->12
-- get in `answers`       | 1      2    11      12
instance applicativeAPoll :: Apply event => Applicative (APoll event) where
  pure a = APoll \e -> applyFlipped a <$> e

instance semigroupAPoll :: (Apply event, Semigroup a) => Semigroup (APoll event a) where
  append = lift2 append

instance monoidAPoll :: (Apply event, Monoid a) => Monoid (APoll event a) where
  mempty = pure mempty

instance heytingAlgebraAPoll :: (Apply event, HeytingAlgebra a) => HeytingAlgebra (APoll event a) where
  tt = pure tt
  ff = pure ff
  not = map not
  implies = lift2 implies
  conj = lift2 conj
  disj = lift2 disj

instance semiringAPoll :: (Apply event, Semiring a) => Semiring (APoll event a) where
  zero = pure zero
  one = pure one
  add = lift2 add
  mul = lift2 mul

instance ringAPoll :: (Apply event, Ring a) => Ring (APoll event a) where
  sub = lift2 sub

-- | Construct a `Poll` from its sampling function.
poll :: forall event a. (forall r. event (a -> r) -> event r) -> APoll event a
poll = APoll

-- | Create a `Poll` which is updated when an `Event` fires, by providing
-- | an initial value.
-- |
-- | ```
-- | push to `cowSupplier`                              |                   C2
-- | push to `surveymonger`                             | C1->1   C1->11          C1->21
-- |                                                    | C2->2   C2->12          C2->22
-- |                                                    | C3->3   C3->13          C3->23
-- | get in `sample (step C1 cowSupplier) surveymonger` | 1       11        _     22
-- | ```
step :: forall event a. IsEvent event => a -> event a -> APoll event a
step a e = APoll (\e0 -> EClass.sampleOnRight ((EClass.once e0 $> a) `alt` e) e0)

-- | Create a `Poll` which is updated when an `Event` fires, by providing
-- | an initial value and a function to combine the current value with a new event
-- | to create a new value.
unfold :: forall event a b. IsEvent event => (b -> a -> b) -> b -> event a -> APoll event b
unfold f a e = step a (fold f a e)

instance Alt event => Alt (APoll event) where
  alt (APoll a) (APoll b) = APoll \e -> a e `alt` b e

instance Plus event => Plus (APoll event) where
  empty = APoll \_ -> empty

-- | Merge together several polls. This has the same functionality
-- | as `oneOf`, but it is faster and less prone to stack explosions.
merge :: forall a. Array (Poll a) → Poll a
merge a = APoll \e -> Event.mergeMap (flip sample e) a

mergeMap :: forall a b. (a -> Poll b) -> Array a → Poll b
mergeMap f a = APoll \e -> Event.mergeMap (flip sample e <<< f) a

-- | A poll where the answers are rigged (influenced/given by) by the nefarious `Event a`
-- | `sham` (fake) because "poll is not fair, because it is controlled by an event"
-- |
-- | # Example
-- |
-- | ```purescript
-- | data Cow = C1 | C2 | C3
-- | test = do
-- |   cowSupplier <- liftST $ create -- List (Cow, Time)
-- |   surveymonger <- liftST $ create -- List (Cow -> MichlenStars, Time)
-- |   let
-- |     (whoWantsToAnswer :: Poll Cow) = sham cowSupplier.event
-- |     (answers :: Event Int) = sample whoWantsToAnswer surveymonger.event
-- |   _ <- subscribe answers \v ->  log $ "answers GOT " <> show v
-- |   cowSupplier.push(...)
-- |   surveymonger.push(...)
-- | ```
-- |
-- | ```
-- | push to `cowSupplier`                           |                   C1             C2
-- | push to `surveymonger`                          | C1->1   C1->11          C1->21
-- |                                                 | C2->2   C2->12          C2->22
-- |                                                 | C3->3   C3->13          C3->23
-- | get in `sample (sham cowSupplier) surveymonger` | _       _         11    _        22
-- | ```
-- |
-- | or
-- |
-- | ```
-- | push to `cowSupplier`                           | C1                      C2             C3
-- | push to `surveymonger`                          |       C1->1   C1->11          C1->21
-- |                                                 |       C2->2   C2->12          C2->22
-- |                                                 |       C3->3   C3->13          C3->23
-- | get in `sample (sham cowSupplier) surveymonger` | _     _       _         12    _        23
-- | ```
-- |
-- | # If sham is listening for sham
-- |
-- | Eta expansion
-- |
-- | ```
-- | x :: Event Int
-- | x = sample_ (sham event1) event1
-- | x = sample (map const $ sham event1) (map applyFlipped event1)
-- | x = sample (map const $ APoll \e -> EClass.sampleOnLeft event1 e) (map applyFlipped event1)
-- | x = EClass.sampleOnLeft event1 (map (_ <<< const) (map applyFlipped event1))
-- | x = EClass.sampleOnLeft event1 (map (const identity) event1 :: Event (Int -> Int))
-- | ```
-- |
-- | Result
-- |
-- | ```
-- | push to `event1`                      | 1     2     3
-- | get in `sample_ (sham event1) event1` | _     2     3
-- | ```
sham :: forall event. IsEvent event => event ~> APoll event
sham i = poll \e -> EClass.sampleOnLeft i e

-- | Turn a function over events into a function over polls.
dredge :: forall a b event. Apply event => (event a -> event b) -> APoll event a -> APoll event b
dredge f (APoll ea) = APoll \eb -> eb <*> f (ea (eb $> identity))

class Pollable event pollable | pollable -> event where
  -- | Sample a `Poll` on some `Event`.
  sample :: forall a r. APoll event a -> pollable (a -> r) -> pollable r

-- `sample poll poll = sampleOnRight poll poll`
-- `sample poll event = poll event`
instance (IsEvent event, Pollable event event) => Pollable event (APoll event) where
  sample = EClass.sampleOnRight
else instance IsEvent event => Pollable event event where
  sample (APoll a) ab = a ab

-- | Sample a `Poll` on some `Event` by providing a combining function.
sampleBy :: forall event pollable a b c. Pollable event pollable => Functor event => Functor pollable => (a -> b -> c) -> APoll event a -> pollable b -> pollable c
sampleBy f apollA pollableB = sample (map f apollA :: APoll event (b -> c)) (map applyFlipped pollableB :: pollable ((b -> c) -> c))

-- | Sample a `Poll` on some `Event`, discarding the event's values.
sample_ :: forall event pollable a b. Pollable event pollable => Functor event => Functor pollable => APoll event a -> pollable b -> pollable a
sample_ = (sampleBy :: (a -> b -> a) -> APoll event a -> pollable b -> pollable a) const

-- | Switch `Poll`s based on an `Event`.
-- |
-- | ```purescript
-- | data Cow = C1 | C2
-- | data Horse = H1 | H2
-- | type Animals = Either Horse Cow
-- | test = do
-- |   pollSupplier <- liftST $ createPure
-- |   surveymonger <- liftST $ createPure
-- |   let
-- |     (onlyCowsAnswer :: Poll Animals) = pure (Right C1) <|> pure (Right C2)
-- |     (onlyHorsesAnswer :: Poll Animals) = pure (Left H1) <|> pure (Left H2)
-- |     (whoWantsToAnswer :: Poll Animals) = switcher onlyCowsAnswer pollSupplier.event
-- |     (answers :: Event Int) = sample whoWantsToAnswer surveymonger.event
-- |   _ <- subscribe answers \v -> do log $ "answers GOT " <> show v
-- |   surveymonger.push(...)
-- |   pollSupplier.push(...)
-- | ```
-- |
-- | ```
-- | push to `pollSupplier` |                         onlyHorsesAnswer
-- | push to `surveymonger` | C1->1     C1->11                             C1->21
-- |                        | C2->2     C2->12                             C2->22
-- |                        | H1->3     H1->13                             H1->23
-- |                        | H2->4     H2->14                             H2->24
-- | get in `answers`       | 1     2   11      12    _                    23       24
-- | ```
switcher :: forall event a. Pollable event event => IsEvent event => APoll event a -> event (APoll event a) -> APoll event a
switcher initialPoll e = poll \s ->
  EClass.keepLatest ((EClass.once s $> (sample initialPoll s)) `alt` map (\p -> sample p s) e)

-- | Sample a `Poll` on some `Event` by providing a predicate function.
gateBy :: forall event p a. Pollable event event => Filterable.Filterable event => (p -> a -> Boolean) -> APoll event p -> event a -> event a
gateBy f ps xs = Filterable.compact (sampleBy (\p x -> if f p x then Just x else Nothing) ps xs)

-- | Filter an `Event` by the boolean value of a `Poll`.
gate :: forall event a. Pollable event event => Filterable.Filterable event => APoll event Boolean -> event a -> event a
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
  => Pollable event event
  => Field t
  => Semiring a
  => (((a -> t) -> t) -> a)
  -> a
  -> APoll event t
  -> APoll event a
  -> APoll event a
integral g initial pollT pollA =
  APoll \e ->
    let
      x = sample pollA (e $> identity)
      y = withLast (sampleBy Tuple pollT x)
      z = fold approx initial y
    in
      EClass.sampleOnRight z e
  where
  approx s { last: Nothing } = s
  approx s { now: Tuple t1 a1, last: Just (Tuple t0 a0) } = s + g (\f -> f (a0 + a1) * (t1 - t0) / two)

  two :: t
  two = one + one

-- | Integrate with respect to some measure of time.
-- |
-- | This function is a simpler version of `integral` where the function being
-- | integrated takes values in the same field used to represent time.
-- |
-- | ```purescript
-- | test = do
-- |   timeSupplier <- liftST $ createPure
-- |   heightSupplier <- liftST $ createPure
-- |   surveymonger <- liftST $ createPure
-- |   let
-- |     (area :: Poll Number) = integral' 0.0 (step 10.0 timeSupplier.event) (step 100.0 heightSupplier.event)
-- |     (answers :: Event Int) = sample area surveymonger.event
-- |   _ <- subscribe answers \v -> do log $ "answers GOT " <> show v
-- |   timeSupplier.push(...)
-- |   heightSupplier.push(...)
-- | ```
-- |
-- | ```
-- | push to `surveymonger`   | show         show           show            show          show
-- | push to `timeSupplier`   |        11.0         13.0                           14.0
-- | push to `heightSupplier` |                                       1.0
-- | get in `answers`         | "0.0"        "100.0"        "300.0"         "300.0"       "301.0"
-- | ```
-- |
-- | ```purescript
-- | (area :: Poll Number) = integral' 0.0 (sham timeSupplier.event) (sham heightSupplier.event)
-- | ```
-- |
-- | ```
-- | push to `surveymonger`   | show         show        show          show   show         show
-- | push to `timeSupplier`   |        1.0                      2.0                  3.0
-- | push to `heightSupplier` |                    1.0
-- | get in `answers`         |                                        "0.0"  "0.0"        "1.0"
-- | ```
integral'
  :: forall event t
   . IsEvent event
  => Pollable event event
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
  => Pollable event event
  => Field t
  => Ring a
  => (((a -> t) -> t) -> a)
  -> APoll event t
  -> APoll event a
  -> APoll event a
derivative g pollT pollA =
  APoll \e ->
    let
      x = sample pollA (e $> identity)
      y = withLast (sampleBy Tuple pollT x)
      z = map approx y
    in
      EClass.sampleOnRight z e
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
  => Pollable event event
  => Field t
  => APoll event t
  -> APoll event t
  -> APoll event t
derivative' = derivative (_ $ identity)

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
solve g a0 pollT f = fixWithInitial a0 \pollA -> integral g a0 pollT (f pollA)

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
solve2 g a0 da0 pollT f =
  fixWithInitial a0 \pollA ->
    integral g a0 pollT
      ( fixWithInitial da0 \pollDa ->
          integral g da0 pollT (f pollA pollDa)
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
   . Poll scene
  -> (scene -> Effect Unit)
  -> Effect (Effect Unit)
animate scenePoll render = do
  { event, unsubscribe } <- animationFrame
  u2 <- subscribe (sample_ scenePoll event) render
  pure do
    unsubscribe
    u2

-- | Turn an ST Ref into a poll
stRefToPoll :: STRef.STRef Global ~> Poll
stRefToPoll r = do
  poll \e -> makeEvent \s -> s e \f -> justOneM do
    i <- STRef.read r
    pure (f i)

-- | Turn an ST Global into a poll
stToPoll :: ST Global ~> Poll
stToPoll r = do
  poll \e -> makeEvent \s -> s e \f -> justOneM do
    i <- r
    pure (f i)

filterMap
  :: forall event a b
   . Filterable.Compactable event
  => Pollable event event
  => Functor event
  => (a -> Maybe b)
  -> APoll event a
  -> APoll event b
filterMap f pollA = poll \e -> Filterable.compact
  $ sampleBy (\a ff -> map ff $ f a) pollA e

partitionMap :: forall event a b c. Pollable event event => Filterable.Compactable event => Functor event => (a -> Either b c) -> APoll event a -> { left :: APoll event b, right :: APoll event c }
partitionMap f pollA = { left: filterMap (either Just (const Nothing)) fb, right: filterMap (either (const Nothing) Just) fb }
  where
  fb = f <$> pollA

instance (Functor event, Filterable.Compactable event, Pollable event event) => Filterable.Compactable (APoll event) where
  compact = filterMap identity
  separate = partitionMap identity

instance (Functor event, Filterable.Compactable event, Pollable event event) => Filterable.Filterable (APoll event) where
  filterMap = filterMap
  filter = filterMap <<< maybeBool
  partitionMap = partitionMap
  partition p xs = do
    let o = partitionMap (eitherBool p) xs
    { no: o.left, yes: o.right }

-- |
-- | # sampleOnRight + pure
-- |
-- | ```purescript
-- | (pollCows :: Poll String) = pure "C1" <|> pure "C2"
-- | (pollCowToOwner :: Poll (String -> String)) = pure (_ <> "_foo") <|> pure (_ <> "_bar")
-- | ```
-- |
-- | ```
-- | push to `surveymonger`                         | (_<>"--1")                     (_<>"--2")                                            (_<>"--3")
-- | get in `sampleOnRight pollCows pollCowToOwner` | "C2_foo--1" "C2_bar--1"        "C2_foo--2" "C2_bar--2"                               "C2_foo--3" "C2_bar--3"
-- | get in `sampleOnLeft pollCows pollCowToOwner`  | _                              "C1_bar--1" "C2_bar--1"                               "C1_bar--2" "C2_bar--2"
-- | get in `flip apply pollCows pollCowToOwner`    | "C1_bar--1" "C2_bar--1"        "C2_foo--1" "C2_bar--1" "C1_bar--2" "C2_bar--2"       "C2_foo--2" "C2_bar--2" "C1_bar--3" "C2_bar--3"
-- | ```
-- |
-- | # sampleOnRight + step
-- |
-- | ```purescript
-- | (pollCows :: Poll String) = step "C1" cowSupplier.event
-- | (pollCowToOwner :: Poll (String -> String)) = step (_ <> "_foo") cowToOwner.event
-- | ```
-- |
-- | ```
-- | push to `surveymonger`                         | (_<>"--1")         (_<>"--2")                 (_<>"--3")                             (_<>"--4")                 (_<>"--5")                                   (_<>"--6")                     (_<>"--7")
-- | push to `cowSupplier`                          |                                                                           "C2"
-- | push to `cowToOwner`                           |                                                                                                                                            (_<>"_bar")
-- | get in `sampleOnRight pollCows pollCowToOwner` | "C1_foo--1"        "C1_foo--2"                "C1_foo--3"                 _          "C2_foo--4"                "C2_foo--5"                _                 "C2_bar--6"                    "C2_bar--7"
-- | get in `sampleOnLeft pollCows pollCowToOwner`  | _                  "C1_foo--1"                "C1_foo--2"                 _          "C2_foo--3"                "C2_foo--4"                _                 "C2_foo--5"                    "C2_bar--6"
-- | get in `flip apply pollCows pollCowToOwner`    | "C1_bar--1"        "C1_foo--1" "C1_foo--2"    "C1_foo--2" "C1_foo--3"     _          "C1_foo--3" "C2_foo--4"    "C2_foo--4" "C2_foo--5"    _                 "C2_bar--5" "C2_bar--6"        "C2_bar--6" "C2_bar--7"
-- | ```
sampleOnRight
  :: forall event a b
   . Pollable event event
  => IsEvent event
  => APoll event a
  -> APoll event (a -> b)
  -> APoll event b
sampleOnRight pollA pollAB = poll \e -> EClass.sampleOnRight (sample_ pollA e) (sampleBy composeFlipped pollAB e)

sampleOnLeft :: forall event a b. Pollable event event => IsEvent event => APoll event a -> APoll event (a -> b) -> APoll event b
sampleOnLeft pollA pollAB = poll \e -> EClass.sampleOnLeft (sample_ pollA e) (sampleBy composeFlipped pollAB e)

-- | Compute a fixed point
-- |
-- | # fixWithInitial + identity
-- |
-- | ```
-- | push to `surveymonger`                                    | C1->1
-- |                                                           | C2->2
-- |                                                           | C3->3
-- | get in `sample (fixWithInitial C1 identity) surveymonger` | 1
-- | ```
-- |
-- | # fixWithInitial + pure
-- |
-- | ```
-- | push to `surveymonger`                                                 | C1->1
-- |                                                                        | C2->2
-- |                                                                        | C3->3
-- | get in `sample (fixWithInitial C1 (\i -> i <|> pure C2)) surveymonger` | 2
-- | ```
-- |
-- | # fixWithInitial + step
-- |
-- | ```
-- | push to `cowSupplier`                                                              | C0                 C3          C4
-- | push to `surveymonger`                                                             |                        C1->1        C1->11
-- |                                                                                    |                        C2->2        C2->12
-- |                                                                                    |                        C3->3        C3->13
-- |                                                                                    |                        C4->4        C4->14
-- | get in `sample (fixWithInitial C1 (\i -> i <|> step C2 cowSupplier)) surveymonger` | _     (subscribe)  _   2       _    14
-- | get in `sample (fixWithInitial C1 (\i -> step C2 cowSupplier <|> i)) surveymonger` | _     (subscribe)  _   1       _    14
-- | ```
-- |
-- | # fixWithInitial + sham
-- |
-- | ```
-- | push to `cowSupplier`                                                              | C0                C2         C3
-- | push to `surveymonger`                                                             |                       C0->0       C0->10
-- |                                                                                    |                       C1->1       C1->11
-- |                                                                                    |                       C2->2       C2->12
-- |                                                                                    |                       C3->3       C3->13
-- |                                                                                    |                       C4->4       C4->14
-- | get in `sample (fixWithInitial C1 (\i -> i <|> sham cowSupplier)) surveymonger`    | _     (subscribe) _   1       _   13
-- | get in `sample (fixWithInitial C1 (\i -> sham cowSupplier <|> i)) surveymonger`    | _     (subscribe) _   1       _   13
-- | ```
fixWithInitial :: forall event a. Pollable event event => IsEvent event => a -> (APoll event a -> APoll event a) -> APoll event a
fixWithInitial a f =
  poll \s ->
    EClass.sampleOnRight
      ( EClass.fix \event ->
          let
            pollA = f (step a event)
          in
            sample_ pollA s
      )
      s

-- | Compute a fixed point
-- |
-- | # Fix + identity
-- |
-- | ```
-- | push to `surveymonger`                      | C1->1
-- |                                             | C2->2
-- |                                             | C3->3
-- | get in `sample (fix identity) surveymonger` | ....nothing
-- | ```
-- |
-- | # Fix + Pure
-- |
-- | ```
-- | push to `surveymonger`                                   | C1->1
-- |                                                          | C2->2
-- |                                                          | C3->3
-- | get in `sample (fix (\i -> i <|> pure C1)) surveymonger` | 1  1  1  1...
-- | ```
-- |
-- | # fix + step
-- |
-- | ```
-- | push to `cowSupplier`                                                | C0                            C2
-- | push to `surveymonger`                                               |                    C0->0           C0->10
-- |                                                                      |                    C1->1           C1->11
-- |                                                                      |                    C2->2           C2->12
-- |                                                                      |                    C3->3           C3->13
-- | get in `sample (fix (\i -> i <|> step C1 cowSupplier)) surveymonger` | _                  1 1 1 1 1 1 1.....
-- | get in `sample (fix (\i -> step C1 cowSupplier <|> i)) surveymonger` | _   (subscribe)    1          _    2 2 2 2 2 2.....
-- | ```
-- |
-- | all cows before first survey are ignored
-- |
-- | ```
-- | push to `cowSupplier`                                                | C0              C2              C3
-- | push to `surveymonger`                                               |                      C0->0           C0->10
-- |                                                                      |                      C1->1           C1->11
-- |                                                                      |                      C2->2           C2->12
-- |                                                                      |                      C3->3           C3->13
-- | get in `sample (fix (\i -> i <|> step C1 cowSupplier)) surveymonger` | _               _    1 1 1 1 1 1 1.....
-- | get in `sample (fix (\i -> step C1 cowSupplier <|> i)) surveymonger` | _   (subscribe) _    1          _    13 3 3 3 3 3 3 3 3.....
-- | ```
-- |
-- | # fix + sham
-- |
-- | ```
-- | push to `cowSupplier`                                             | C0               C1           C2
-- | push to `surveymonger`                                            |                       C0->0
-- |                                                                   |                       C1->1
-- |                                                                   |                       C2->2
-- |                                                                   |                       C3->3
-- |                                                                   |                       C4->4
-- | get in `sample (fix (\i -> i <|> sham cowSupplier)) surveymonger` | _   (subscribe)  _    _       2 2 2 2 2 2 2.....
-- | get in `sample (fix (\i -> sham cowSupplier <|> i)) surveymonger` | _   (subscribe)  _    _       2 2 2 2 2 2 2.....
-- | ```
fix
  :: forall event a
   . Pollable event event
  => IsEvent event
  => (APoll event a -> APoll event a)
  -> APoll event a
fix f = poll
  let
    innerPoll :: forall r. event (a -> r) -> event r
    innerPoll e =

      let
        fixed :: event (Tuple a (a -> r))
        fixed = EClass.fix \ee -> sampleBy Tuple (f (sham (fst <$> ee)) :: APoll event a) e
      in
        (\(Tuple a ff) -> ff a) <$> fixed
  in
    innerPoll

-- | # Once + pure
-- |
-- | ```
-- | push to `surveymonger`                                    | C1->1       C1->11
-- |                                                           | C2->2       C2->12
-- | get in `sample (once (pure C1 <|> pure C2)) surveymonger` | 1           _
-- | ```
-- |
-- | # Once + sham
-- |
-- | ```
-- | push to `cowSupplier`                                  | C1                      C2             C3
-- | push to `surveymonger`                                 |       C1->1   C1->11          C1->21
-- | ```
-- |                                                        |       C2->2   C2->12          C2->22
-- |                                                        |       C3->3   C3->13          C3->23
-- | get in `sample (once (sham cowSupplier)) surveymonger` | _     _       _         12    _        _
-- | ```
-- |
-- | # Once + step
-- |
-- | ```
-- | push to `cowSupplier`                                     |                   C2
-- | push to `surveymonger`                                    | C1->1   C1->11          C1->21
-- |                                                           | C2->2   C2->12          C2->22
-- |                                                           | C3->3   C3->13          C3->23
-- | get in `sample (once (step C1 cowSupplier)) surveymonger` | 1       _         _     _
-- | ```
once :: forall event a. Pollable event event => IsEvent event => APoll event a -> APoll event a
once a = poll \e -> EClass.once (sample a e)

instance (IsEvent event, Plus event, Pollable event event) => IsEvent (APoll event) where
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

createTagged
  :: forall a
   . String
  -> ST Global (PollIO a)
createTagged tag = do
  { event, push } <- Event.createTagged tag
  { poll: p } <- rant (sham event) -- TODO: why not export unsubscribe?
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

mailboxS
  :: forall b
   . ST Global { push :: { address :: String, payload :: b } -> Effect Unit, poll :: String -> Poll b }
mailboxS = do
  { push, event } <- Event.mailboxS
  pure { poll: map sham event, push }

-- | Once an event comes in, it just goes on and on and on and on (a rant) without listening to future events.
-- | Rant ignores the initial event and broadcasts all the others.
-- |
-- | # rant + pure
-- |
-- | ```
-- | push to `surveymonger`                                    | C1->1     C1->11
-- |                                                           | C2->2     C2->12
-- | get in `sample (rant (pure C1 <|> pure C2)) surveymonger` | _         _
-- | ```
-- |
-- | # rant + sham
-- |
-- | ```
-- | push to `cowSupplier`                                        | C0             C1                    C2             C3
-- | push to `surveymonger`                                       |                    C1->1   C1->11          C1->21
-- |                                                              |                    C2->2   C2->12          C2->22
-- |                                                              |                    C3->3   C3->13          C3->23
-- | get in `sample (rant (sham cowSupplier.event)) surveymonger` | _ (subscribe)  _   _       _         12    _        23
-- | ```
-- |
-- | `unsubscribe` makes poll ignore `cowSupplier`
-- |
-- | ```
-- | push to `cowSupplier`                                        | C0             C1                    C2                           C3
-- | push to `surveymonger`                                       |                    C1->1   C1->11          C1->21
-- |                                                              |                    C2->2   C2->12          C2->22
-- |                                                              |                    C3->3   C3->13          C3->23
-- | get in `sample (rant (sham cowSupplier.event)) surveymonger` | _ (subscribe)  _   _       _         12    _       (unsubscribe)  _
-- | ```
-- |
-- | # rant + step
-- |
-- | ```
-- | push to `cowSupplier`                                           | C0             C2                    C3
-- | push to `surveymonger`                                          |                    C1->1   C1->11          C1->21
-- |                                                                 |                    C2->2   C2->12          C2->22
-- |                                                                 |                    C3->3   C3->13          C3->23
-- | get in `sample (rant (step C1 cowSupplier.event)) surveymonger` | _  (subscribe) _   _       _         _     _
-- | get in `...` (but without `EClass.once requesterEvent`)         | _  (subscribe) _   _       1         _     13
-- | ```
rant
  :: forall a
   . Poll a
  -> ST Global { poll :: Poll a, unsubscribe :: ST Global Unit }
rant a = do
  ep <- Event.createPure
  maybeUnsubscribe <- STRef.new Nothing
  pure
    { unsubscribe: do
        STRef.read maybeUnsubscribe >>= case _ of
          Nothing -> pure unit
          Just unsubscribe -> unsubscribe
    , poll: poll \requesterEvent -> makeEvent \responseEventCallback -> do
        STRef.read maybeUnsubscribe >>= case _ of
          Nothing -> do
            unsubscribe <- responseEventCallback (sample_ a (EClass.once requesterEvent) :: Event a) \(i :: a) -> justNone (ep.push i)
            void $ flip STRef.write maybeUnsubscribe (Just unsubscribe)
          Just _ -> pure unit
        u3 <- responseEventCallback (EClass.sampleOnRightOp requesterEvent ep.event) justOne
        pure u3
    }

-- | Deflect saves all initial events (events from `pure` for example) and ignores others
-- | Rant and deflect can be considered opposites.
-- |
-- | # Deflect + pure
-- |
-- | ```
-- | push to `surveymonger`                                       | C1->1       C1->11
-- |                                                              | C2->2       C2->12
-- | get in `sample (deflect (pure C1 <|> pure C2)) surveymonger` | 1      2    11      12
-- | ```
-- |
-- | # Deflect + sham
-- |
-- | ```
-- | push to `cowSupplier`                                     | C1                      C2             C3
-- | push to `surveymonger`                                    |       C1->1   C1->11          C1->21
-- |                                                           |       C2->2   C2->12          C2->22
-- |                                                           |       C3->3   C3->13          C3->23
-- | get in `sample (deflect (sham cowSupplier)) surveymonger` | _     _       _         _     _        _
-- | ```
-- |
-- | # Deflect + step
-- |
-- | ```
-- | push to `cowSupplier`                                        | C0             C2                    C3
-- | push to `surveymonger`                                       |                    C1->1   C1->11          C1->21
-- |                                                              |                    C2->2   C2->12          C2->22
-- |                                                              |                    C3->3   C3->13          C3->23
-- | get in `sample (deflect (step C1 cowSupplier)) surveymonger` | _  (subscribe) _   1       11        _     21
-- | ```
deflect
  :: forall a
   . Poll a
  -> ST Global (Poll a)
deflect pollA = do
  ep <- STRef.new []
  maybeUnsubscribe <- STRef.new Nothing
  let
    innerPoll :: forall r. Event (a -> r) -> Event r
    innerPoll = \e ->
      let
        innerEvent :: (forall x. Event x -> (x -> EventfulProgram r) -> ST Global (ST Global Unit)) -> ST Global (ST Global Unit)
        innerEvent s = do
          STRef.read maybeUnsubscribe >>= case _ of
            Nothing -> do
              unsubscribe <- (s :: Event a -> (a -> ProgramfulEvent Unit) -> ST Global (ST Global Unit)) (sample_ pollA (EClass.once e) :: Event a) \i -> justNone do
                void $ liftST $ flip STRef.modify ep $ flip Array.snoc i
              void $ STRef.write (Just unsubscribe) maybeUnsubscribe
            Just _ -> do
              pure unit
          u3 <- (s :: Event (a -> r) -> ((a -> r) -> EventfulProgram r) -> ST Global (ST Global Unit)) (e :: Event (a -> r)) \f -> justManyM do
            STRef.read maybeUnsubscribe >>= case _ of
              Nothing -> do
                pure unit
              Just unsubscribe -> do
                unsubscribe
            r <- STRef.read ep
            pure $ map f r
          pure do
            u3
      in
        makeEvent innerEvent
  pure $ poll innerPoll

data KeepLatestOrder event a b
  = KeepLatestStart (APoll event a) (a -> b)
  | KeepLatestLast b

keepLatest
  :: forall event a
   . Filterable.Filterable event
  => EClass.IsEvent event
  => Pollable event event
  => APoll event (APoll event a)
  -> APoll event a
keepLatest a = APoll \e ->
  Filterable.filterMap
    ( case _ of
        KeepLatestLast b -> Just b
        _ -> Nothing
    ) $ EClass.fix \ie -> oneOf
    [ sampleBy KeepLatestStart a e
    , EClass.keepLatest $ flip Filterable.filterMap ie case _ of
        KeepLatestStart b ff -> Just (sampleBy (\bb _ -> KeepLatestLast (ff bb)) b (EClass.once ie))
        _ -> empty
    ]

listen_
  :: forall a
   . Poll a
  -> (a -> Effect Unit)
  -> Effect (Effect Unit)
listen_ p f = do
  { event, push } <- liftST Event.create
  Event.subscribe (sample_ p event) f <* push unit
