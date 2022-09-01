module FRP.Event
  ( Backdoor(..)
  , Bus(..)
  , BusT
  , Create(..)
  , CreateT
  , Delay(..)
  , DelayT
  , Event
  , EventIO
  , Hot(..)
  , HotT
  , Mailboxed(..)
  , MailboxedT
  , MakeEvent(..)
  , MakeEventT
  , Memoize(..)
  , MemoizeT
  , Subscribe(..)
  , SubscribeT
  , backdoor
  , burning
  , bus
  , create
  , delay
  , hot
  , mailboxed
  , makeEvent
  , memoize
  , module Class
  , subscribe
  ) where

import Prelude

import Control.Alt ((<|>))
import Control.Alternative (class Alt, class Alternative, class Plus)
import Control.Apply (lift2)
import Control.Monad.ST.Class (liftST)
import Data.Array (deleteBy, length)
import Data.Array as Array
import Data.Array.ST as STArray
import Data.Compactable (class Compactable)
import Data.Either (Either(..), either, hush)
import Data.Filterable as Filterable
import Data.Foldable (for_, traverse_)
import Data.HeytingAlgebra (ff, implies, tt)
import Data.Map as Map
import Data.Maybe (Maybe(..), maybe)
import Data.Monoid.Action (class Action)
import Data.Monoid.Additive (Additive(..))
import Data.Set (Set, singleton, delete)
import Effect (Effect, foreachE)
import Effect.Ref as ERef
import Effect.Ref as Ref
import Effect.Timer (TimeoutId, clearTimeout, setTimeout)
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
newtype Event a = AnEvent ((a -> Effect Unit) -> Effect (Effect Unit))

instance functorEvent :: Functor Event where
  map f (AnEvent e) = AnEvent \k -> e (k <<< f)

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

instance filterableEvent :: Filterable.Filterable Event where
  filter = filter'
  filterMap = filter
  partition p xs = { yes: filter' p xs, no: filter' (not <<< p) xs }
  partitionMap f xs =
    { left: Filterable.filterMap (either Just (const Nothing) <<< f) xs
    , right: Filterable.filterMap (hush <<< f) xs
    }

instance altEvent :: Alt Event where
  alt (AnEvent f) (AnEvent g) =
    AnEvent \k -> do
      c1 <- f k
      c2 <- g k
      pure do
        c1
        c2

instance plusEvent :: Plus Event where
  empty = AnEvent \_ -> pure (pure unit)

instance applyEvent :: Apply Event where
  apply a b = biSampleOn a ((#) <$> b)

instance applicativeEvent :: Applicative Event where
  pure a = AnEvent \k -> pure unit <$ k a

instance alternativeEvent :: Alternative Event

instance eventIsEvent :: Class.IsEvent Event where
  fold = fold
  keepLatest = keepLatest
  sampleOn = sampleOn
  fix = fix

instance semigroupEvent :: (Semigroup a) => Semigroup (Event a) where
  append = lift2 append

instance monoidEvent :: (Monoid a) => Monoid (Event a) where
  mempty = pure mempty

instance heytingAlgebraEvent :: (HeytingAlgebra a) => HeytingAlgebra (Event a) where
  tt = pure tt
  ff = pure ff
  not = map not
  implies = lift2 implies
  conj = lift2 conj
  disj = lift2 disj

instance semiringEvent :: (Semiring a) => Semiring (Event a) where
  zero = pure zero
  one = pure one
  add = lift2 add
  mul = lift2 mul

instance ringEvent :: (Ring a) => Ring (Event a) where
  sub = lift2 sub

-- | Fold over values received from some `Event`, creating a new `Event`.
fold :: forall a b. (a -> b -> b) -> Event a -> b -> Event b
fold f (AnEvent e) b =
  AnEvent \k -> do
    result <- (Ref.new b)
    e \a -> do
      o <- Ref.modify (f a) result
      k o

-- | Create an `Event` which only fires when a predicate holds.
filter :: forall a b. (a -> Maybe b) -> Event a -> Event b
filter p (AnEvent e) =
  AnEvent \k ->
    e \a -> case p a of
      Just y -> k y
      Nothing -> pure unit

-- | Create an `Event` which samples the latest values from the first event
-- | at the times when the second event fires.
sampleOn :: forall a b. Event a -> Event (a -> b) -> Event b
sampleOn (AnEvent e1) (AnEvent e2) =
  AnEvent \k -> do
    latest <- Ref.new Nothing
    c1 <-
      e1 \a -> do
        Ref.write (Just a) latest
    c2 <-
      e2 \f -> do
        o <- Ref.read latest
        traverse_ (k <<< f) o
    pure do
      c1
      c2

biSampleOn :: forall a b. Event a -> Event (a -> b) -> Event b
biSampleOn (AnEvent e1) (AnEvent e2) =
  AnEvent \k -> do
    latest1 <- Ref.new Nothing
    replay1 <- liftST $ STArray.new
    latest2 <- Ref.new Nothing
    replay2 <- liftST $ STArray.new
    -- First we capture the immediately emitted events
    capturing <- Ref.new true
    c1 <-
      e1 \a -> do
        c <- Ref.read capturing
        if c then void $ liftST $ STArray.push a replay1
        else do
          _ <- Ref.write (Just a) latest1
          l <- Ref.read latest2
          traverse_ (\f -> k (f a)) l
    c2 <-
      e2 \f -> do
        c <- Ref.read capturing
        if c then void $ liftST $ STArray.push f replay2
        else do
          _ <- Ref.write (Just f) latest2
          l <- Ref.read latest1
          traverse_ (\a -> k (f a)) l
    -- And then we replay them according to the `Applicative Array` instance
    _ <- Ref.write false capturing
    samples1 <- liftST $ STArray.freeze replay1
    samples2 <- liftST $ STArray.freeze replay2
    case samples1 of
      -- if there are no samples in samples1, we still want to write samples2
      [] -> Ref.write (Array.last samples2) latest2
      _ -> for_ samples1 \a -> do
        -- We write the current values as we go through -- this would only matter for recursive events
        _ <- Ref.write (Just a) latest1
        for_ samples2 \f -> do
          _ <- Ref.write (Just f) latest2
          k (f a)
    -- Free the samples so they can be GCed
    _ <- liftST $ STArray.splice 0 (length samples1) [] replay1
    _ <- liftST $ STArray.splice 0 (length samples2) [] replay2
    pure do
      c1
      c2

-- | Flatten a nested `Event`, reporting values only from the most recent
-- | inner `Event`.
keepLatest :: forall a. Event (Event a) -> Event a
keepLatest (AnEvent e) =
  AnEvent \k -> do
    cancelInner <- Ref.new (pure unit)
    cancelOuter <-
      e \inner -> do
        ci <- Ref.read cancelInner
        ci
        c <- subscribe inner k
        Ref.write c cancelInner
    pure do
      ci <- Ref.read cancelInner
      ci
      cancelOuter

-- | Compute a fixed point
fix :: forall i o. (Event i -> { input :: Event i, output :: Event o }) -> Event o
fix f =
  AnEvent \k -> do
    { event, push } <- create
    let { input, output } = f event
    c1 <- subscribe input push
    c2 <- subscribe output k
    pure do
      c1
      c2

-- | Subscribe to an `Event` by providing a callback.
-- |
-- | `subscribe` returns a canceller function.
subscribe :: SubscribeT
subscribe i = (\(Subscribe nt) -> nt) backdoor.subscribe i

type SubscribeT =
  forall a
   . Event a
  -> (a -> Effect Unit)
  -> Effect (Effect Unit)

newtype Subscribe = Subscribe SubscribeT

type MakeEventT =
  forall a
   . ((a -> Effect Unit) -> Effect (Effect Unit))
  -> Event a

newtype MakeEvent = MakeEvent MakeEventT

-- | Make an `Event` from a function which accepts a callback and returns an
-- | unsubscription function.
-- |
-- | Note: you probably want to use `create` instead, unless you need explicit
-- | control over unsubscription.
makeEvent :: MakeEventT
makeEvent i = (\(MakeEvent nt) -> nt) backdoor.makeEvent i

type EventIO a =
  { event :: Event a
  , push :: a -> Effect Unit
  }

-- | Create an event and a function which supplies a value to that event.
create :: CreateT
create = do
  pure unit
  (\(Create nt) -> nt) backdoor.create

type CreateT =
  forall a
   . Effect (EventIO a)

newtype Create = Create CreateT

-- | Creates an event bus within a closure.
bus :: BusT
bus i = (\(Bus nt) -> nt) backdoor.bus i

type BusT = forall r a. ((a -> Effect Unit) -> Event a -> r) -> Event r
newtype Bus = Bus BusT

-- | Takes the entire domain of a and allows for ad-hoc specialization.
mailboxed :: MailboxedT
mailboxed i = (\(Mailboxed nt) -> nt) backdoor.mailboxed i

type MailboxedT = forall r a b. Ord a => Event { address :: a, payload :: b } -> ((a -> Event b) -> r) -> Event r

newtype Mailboxed = Mailboxed MailboxedT

-- | Takes an event and memoizes it within a closure.
-- | All interactions with the event in the closure will not trigger a fresh
-- | subscription. Outside the closure does, however, trigger a fresh subscription.
memoize :: MemoizeT
memoize i = (\(Memoize nt) -> nt) backdoor.memoize i

type MemoizeT = forall r a. Event a -> (Event a -> r) -> Event r
newtype Memoize = Memoize MemoizeT

-- | Makes an event hot, meaning that it will start firing on left-bind. This means that `pure` should never be used with `hot` as it will be lost. Use this for loops, for example.
hot :: HotT
hot i = (\(Hot nt) -> nt) backdoor.hot i

type HotT =
  forall a
   . Event a
  -> Effect { event :: Event a, unsubscribe :: Effect Unit }

newtype Hot = Hot HotT

-- | Makes an event _burning_ hot. Like hot, it will start firing immediately on left bind. In addition, it _always_ fires _immediately_ upon subscription with the most recent value.
burning
  :: forall a
   . a
  -> Event a
  -> Effect { event :: Event a, unsubscribe :: Effect Unit }
burning i e = do
  r <- Ref.new i
  { event, push } <- create
  unsubscribe <- subscribe e \x -> do
    _ <- Ref.write x r
    push x
  pure
    { event: event <|> makeEvent \k ->
        do
          o <- Ref.read r
          k o
          pure (pure unit)
    , unsubscribe
    }

--
instance Action (Additive Int) (Event a) where
  act (Additive i) = delay i

delay :: DelayT
delay i = (\(Delay nt) -> nt) backdoor.delay i

type DelayT = forall a. Int -> Event a -> Event a
newtype Delay = Delay DelayT

type Backdoor =
  { makeEvent :: MakeEvent
  , create :: Create
  , subscribe :: Subscribe
  , bus :: Bus
  , memoize :: Memoize
  , hot :: Hot
  , mailboxed :: Mailboxed
  , delay :: Delay
  }

backdoor :: Backdoor
backdoor =
  { makeEvent:
      let
        makeEvent_ :: MakeEvent
        makeEvent_ = MakeEvent AnEvent
      in
        makeEvent_
  , create:
      let
        create_ :: Create
        create_ = Create do
          subscribers <- Ref.new []
          pure
            { event:
                AnEvent \k -> do
                  _ <- Ref.modify (_ <> [ k ]) subscribers
                  pure do
                    _ <- Ref.modify (deleteBy unsafeRefEq k) subscribers
                    pure unit
            , push:
                \a -> do
                  subs <- Ref.read subscribers
                  foreachE subs \k -> k a
            }
      in
        create_
  , subscribe:
      let
        subscribe_ :: Subscribe
        subscribe_ = Subscribe \(AnEvent e) k -> e k
      in
        subscribe_
  , bus:
      let
        bus_ :: Bus
        bus_ = Bus \f -> makeEvent \k -> do
          { push, event } <- create
          k (f push event)
          pure (pure unit)
      in
        bus_
  , memoize:
      let
        memoize_ :: Memoize
        memoize_ = Memoize \e f -> makeEvent \k -> do
          { push, event } <- create
          k (f event)
          subscribe e push
      in
        memoize_
  , hot:
      let
        hot_ :: Hot
        hot_ = Hot \e -> do
          { event, push } <- create
          unsubscribe <- subscribe e push
          pure { event, unsubscribe }
      in
        hot_
  , mailboxed:
      let
        mailboxed_ :: Mailboxed
        mailboxed_ = Mailboxed \e f -> makeEvent \k1 -> do
          r <- Ref.new Map.empty
          k1 $ f \a -> makeEvent \k2 -> do
            Ref.modify_
              ( Map.alter
                  ( case _ of
                      Nothing -> Just [ k2 ]
                      Just arr -> Just (arr <> [ k2 ])
                  )
                  a
              )
              r
            pure $ Ref.modify_
              ( Map.alter
                  ( case _ of
                      Nothing -> Nothing
                      Just arr -> Just (deleteBy unsafeRefEq k2 arr)
                  )
                  a
              )
              r
          unsub <- subscribe e \{ address, payload } -> do
            o <- Ref.read r
            case Map.lookup address o of
              Nothing -> pure unit
              Just arr -> for_ arr (_ $ payload)
          pure do
            -- free references - helps gc?
            Ref.write (Map.empty) r
            unsub
      in
        mailboxed_
  , delay:
      let
        delay_ :: Delay
        delay_ = Delay \n e ->
          makeEvent \k -> do
            tid <- ERef.new (mempty :: Set TimeoutId)
            canceler <-
              subscribe e \a -> do
                localId <- ERef.new Nothing
                id <-
                  setTimeout n do
                    k a
                    lid <- ERef.read localId
                    maybe (pure unit) (\id -> ERef.modify_ (delete id) tid) lid
                ERef.write (Just id) localId
                ERef.modify_ (append (singleton id)) tid
            pure do
              ids <- ERef.read tid
              for_ ids clearTimeout
              canceler
      in
        delay_
  }
