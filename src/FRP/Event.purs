module FRP.Event
  ( AnEvent
  , Event
  , AnEventIO
  , EventIO
  , backdoor
  , Backdoor(..)
  , create
  , CreateT
  , Create(..)
  , makeEvent
  , MakeEventT
  , MakeEvent(..)
  , subscribe
  , SubscribeT
  , Subscribe(..)
  , bus
  , BusT
  , Bus(..)
  , memoize
  , MemoizeT
  , Memoize(..)
  , burning
  , hot
  , HotT
  , Hot(..)
  , mailboxed
  , MailboxedT
  , Mailboxed(..)
  , delay
  , DelayT
  , Delay(..)
  , ToEvent
  , toEvent
  , FromEvent
  , fromEvent
  , module Class
  ) where

import Prelude

import Control.Alt ((<|>))
import Control.Alternative (class Alt, class Alternative, class Plus)
import Control.Apply (lift2)
import Control.Monad.ST.Class (class MonadST, liftST)
import Control.Monad.ST.Global (Global)
import Control.Monad.ST.Internal (ST)
import Control.Monad.ST.Internal as Ref
import Data.Array (deleteBy, length)
import Data.Array as Array
import Data.Array.ST as STArray
import Data.Compactable (class Compactable)
import Data.Either (Either(..), either, hush)
import Data.Filterable as Filterable
import Data.Foldable (for_, sequence_, traverse_)
import Data.HeytingAlgebra (ff, implies, tt)
import Data.Map as Map
import Data.Maybe (Maybe(..), maybe)
import Data.Monoid.Action (class Action)
import Data.Monoid.Additive (Additive(..))
import Data.Monoid.Always (class Always, always)
import Data.Monoid.Endo (Endo(..))
import Data.Newtype (unwrap)
import Data.Profunctor (dimap)
import Data.Set (Set, singleton, delete)
import Effect (Effect)
import Effect.Ref as ERef
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
newtype AnEvent m a = AnEvent ((a -> m Unit) -> m (m Unit))
type Event a = AnEvent Effect a

type STEvent a = AnEvent (ST Global) a

instance functorEvent :: Functor (AnEvent m) where
  map f (AnEvent e) = AnEvent \k -> e (k <<< f)

instance compactableEvent :: Applicative m => Compactable (AnEvent m) where
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

filter' :: forall m a. Applicative m => (a → Boolean) → AnEvent m a → AnEvent m a
filter' f =
  filter
    ( \a -> case f a of
        true -> Just a
        false -> Nothing
    )

instance filterableEvent :: Applicative m => Filterable.Filterable (AnEvent m) where
  filter = filter'
  filterMap = filter
  partition p xs = { yes: filter' p xs, no: filter' (not <<< p) xs }
  partitionMap f xs =
    { left: Filterable.filterMap (either Just (const Nothing) <<< f) xs
    , right: Filterable.filterMap (hush <<< f) xs
    }

instance altEvent :: Applicative m => Alt (AnEvent m) where
  alt (AnEvent f) (AnEvent g) =
    AnEvent \k -> ado
      c1 <- f k
      c2 <- g k
      in (c1 *> c2)

instance plusEvent :: Applicative m => Plus (AnEvent m) where
  empty = AnEvent \_ -> pure (pure unit)

instance applyEvent :: MonadST s m => Apply (AnEvent m) where
  apply a b = biSampleOn a ((#) <$> b)

instance applicativeEvent :: MonadST s m => Applicative (AnEvent m) where
  pure a = AnEvent \k -> pure unit <$ k a

instance alternativeEvent :: MonadST s m => Alternative (AnEvent m)

instance eventIsEvent :: MonadST s m => Class.IsEvent (AnEvent m) where
  fold = fold
  keepLatest = keepLatest
  sampleOn = sampleOn
  fix = fix

instance semigroupEvent :: (Semigroup a, MonadST s m) => Semigroup (AnEvent m a) where
  append = lift2 append

instance monoidEvent :: (Monoid a, MonadST s m) => Monoid (AnEvent m a) where
  mempty = pure mempty

instance heytingAlgebraEvent :: (HeytingAlgebra a, MonadST s m) => HeytingAlgebra (AnEvent m a) where
  tt = pure tt
  ff = pure ff
  not = map not
  implies = lift2 implies
  conj = lift2 conj
  disj = lift2 disj

instance semiringEvent :: (Semiring a, MonadST s m) => Semiring (AnEvent m a) where
  zero = pure zero
  one = pure one
  add = lift2 add
  mul = lift2 mul

instance ringEvent :: (Ring a, MonadST s m) => Ring (AnEvent m a) where
  sub = lift2 sub

-- | Fold over values received from some `Event`, creating a new `Event`.
fold :: forall m s a b. MonadST s m => (a -> b -> b) -> AnEvent m a -> b -> AnEvent m b
fold f (AnEvent e) b =
  AnEvent \k -> do
    result <- liftST (Ref.new b)
    e \a -> liftST (Ref.modify (f a) result) >>= k

-- | Create an `Event` which only fires when a predicate holds.
filter :: forall m a b. Applicative m => (a -> Maybe b) -> AnEvent m a -> AnEvent m b
filter p (AnEvent e) =
  AnEvent \k ->
    e \a -> case p a of
      Just y -> k y
      Nothing -> pure unit

-- | Create an `Event` which samples the latest values from the first event
-- | at the times when the second event fires.
sampleOn :: forall m s a b. MonadST s m => Applicative m => AnEvent m a -> AnEvent m (a -> b) -> AnEvent m b
sampleOn (AnEvent e1) (AnEvent e2) =
  AnEvent \k -> do
    latest <- liftST $ Ref.new Nothing
    c1 <-
      e1 \a -> do
        liftST $ void $ Ref.write (Just a) latest
    c2 <-
      e2 \f -> do
        (liftST $ Ref.read latest) >>= traverse_ (k <<< f)
    pure (c1 *> c2)

biSampleOn :: forall m s a b. MonadST s m => Applicative m => AnEvent m a -> AnEvent m (a -> b) -> AnEvent m b
biSampleOn (AnEvent e1) (AnEvent e2) =
  AnEvent \k -> do
    latest1 <- liftST $ Ref.new Nothing
    replay1 <- liftST $ STArray.new
    latest2 <- liftST $ Ref.new Nothing
    replay2 <- liftST $ STArray.new
    -- First we capture the immediately emitted events
    capturing <- liftST $ Ref.new true
    c1 <-
      e1 \a -> do
        (liftST $ Ref.read capturing) >>=
          if _ then liftST $ void $ STArray.push a replay1
          else do
            _ <- liftST $ Ref.write (Just a) latest1
            (liftST $ Ref.read latest2) >>= traverse_ (\f -> k (f a))
    c2 <-
      e2 \f -> do
        (liftST $ Ref.read capturing) >>=
          if _ then liftST $ void $ STArray.push f replay2
          else do
            _ <- liftST $ Ref.write (Just f) latest2
            (liftST $ Ref.read latest1) >>= traverse_ (\a -> k (f a))
    -- And then we replay them according to the `Applicative Array` instance
    _ <- liftST $ Ref.write false capturing
    samples1 <- liftST $ STArray.freeze replay1
    samples2 <- liftST $ STArray.freeze replay2
    case samples1 of
      -- if there are no samples in samples1, we still want to write samples2
      [] -> void $ liftST $ Ref.write (Array.last samples2) latest2
      _ -> for_ samples1 \a -> do
          -- We write the current values as we go through -- this would only matter for recursive events
          _ <- liftST $ Ref.write (Just a) latest1
          for_ samples2 \f -> do
            _ <- liftST $ Ref.write (Just f) latest2
            k (f a)
    -- Free the samples so they can be GCed
    _ <- liftST $ STArray.splice 0 (length samples1) [] replay1
    _ <- liftST $ STArray.splice 0 (length samples2) [] replay2
    pure (c1 *> c2)

-- | Flatten a nested `Event`, reporting values only from the most recent
-- | inner `Event`.
keepLatest :: forall m s a. MonadST s m => AnEvent m (AnEvent m a) -> AnEvent m a
keepLatest (AnEvent e) =
  AnEvent \k -> do
    cancelInner <- liftST $ Ref.new Nothing
    cancelOuter <-
      e \inner -> do
        (liftST $ Ref.read cancelInner) >>= sequence_
        c <- subscribe inner k
        liftST $ void $ Ref.write (Just c) cancelInner
    pure do
      (liftST $ Ref.read cancelInner) >>= sequence_
      cancelOuter

-- | Compute a fixed point
fix :: forall m s i o. MonadST s m => Monad m => (AnEvent m i -> { input :: AnEvent m i, output :: AnEvent m o }) -> AnEvent m o
fix f =
  AnEvent \k -> do
    { event, push } <- create
    let { input, output } = f event
    c1 <- subscribe input push
    c2 <- subscribe output k
    pure (c1 *> c2)

-- | Subscribe to an `Event` by providing a callback.
-- |
-- | `subscribe` returns a canceller function.
subscribe :: SubscribeT
subscribe i = (\(Subscribe nt) -> nt) backdoor.subscribe i

type SubscribeT =
  forall m a
   . AnEvent m a
  -> (a -> m Unit)
  -> m (m Unit)

newtype Subscribe = Subscribe SubscribeT

type MakeEventT =
  forall m a
   . ((a -> m Unit) -> m (m Unit))
  -> AnEvent m a

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

type AnEventIO m a =
  { event :: AnEvent m a
  , push :: a -> m Unit
  }

-- | Create an event and a function which supplies a value to that event.
create :: CreateT
create = do
  pure unit
  (\(Create nt) -> nt) backdoor.create

type CreateT =
  forall m1 m2 s a
   . MonadST s m1
  => MonadST s m2
  => m1 (AnEventIO m2 a)

newtype Create = Create CreateT

-- | Creates an event bus within a closure.
bus :: BusT
bus i = (\(Bus nt) -> nt) backdoor.bus i

type BusT = forall m s r a. MonadST s m => ((a -> m Unit) -> AnEvent m a -> r) -> AnEvent m r
newtype Bus = Bus BusT

-- | Takes the entire domain of a and allows for ad-hoc specialization.
mailboxed :: MailboxedT
mailboxed i = (\(Mailboxed nt) -> nt) backdoor.mailboxed i

type MailboxedT = forall m s r a b. Ord a => MonadST s m => AnEvent m { address :: a, payload :: b } -> ((a -> AnEvent m b) -> r) -> AnEvent m r

newtype Mailboxed = Mailboxed MailboxedT

-- | Takes an event and memoizes it within a closure.
-- | All interactions with the event in the closure will not trigger a fresh
-- | subscription. Outside the closure does, however, trigger a fresh subscription.
memoize :: MemoizeT
memoize i = (\(Memoize nt) -> nt) backdoor.memoize i

type MemoizeT = forall m s r a. MonadST s m => AnEvent m a -> (AnEvent m a -> r) -> AnEvent m r
newtype Memoize = Memoize MemoizeT

-- | Makes an event hot, meaning that it will start firing on left-bind. This means that `pure` should never be used with `hot` as it will be lost. Use this for loops, for example.
hot :: HotT
hot i = (\(Hot nt) -> nt) backdoor.hot i

type HotT =
  forall m s a
   . MonadST s m
  => AnEvent m a
  -> m { event :: AnEvent m a, unsubscribe :: m Unit }

newtype Hot = Hot HotT

-- | Makes an event _burning_ hot. Like hot, it will start firing immediately on left bind. In addition, it _always_ fires _immediately_ upon subscription with the most recent value.
burning
  :: forall m s a
   . MonadST s m
  => a
  -> AnEvent m a
  -> m { event :: AnEvent m a, unsubscribe :: m Unit }
burning i e = do
  r <- liftST $ Ref.new i
  { event, push } <- create
  unsubscribe <- subscribe e \x -> do
    _ <- liftST $ Ref.write x r
    push x
  pure
    { event: event <|> makeEvent \k -> do
        liftST (Ref.read r) >>= k
        pure (pure unit)
    , unsubscribe
    }

--
instance Action (Additive Int) (Event a) where
  act (Additive i) = delay i

instance Action (Additive Int) (STEvent a) where
  act = const identity

delay :: DelayT
delay i = (\(Delay nt) -> nt) backdoor.delay i

type DelayT = forall a. Int -> Event a -> Event a
newtype Delay = Delay DelayT

type ToEvent m a =
  Always (Endo Function (Effect Unit)) (Endo Function (m Unit))
  => Always (m (m Unit)) (Effect (Effect Unit))
  => Applicative m
  => a

toEvent :: forall m. ToEvent m (AnEvent m ~> Event)
toEvent (AnEvent i) = AnEvent $
  dimap
    (\f a -> unwrap (always (Endo (const (f a))) :: Endo Function (m Unit)) (pure unit))
    (always :: m (m Unit) -> Effect (Effect Unit))
    i

type FromEvent m a =
  Always (m Unit) (Effect Unit)
  => Always (Endo Function (Effect (Effect Unit))) (Endo Function (m (m Unit)))
  => Applicative m
  => a

fromEvent :: forall m. FromEvent m (Event ~> AnEvent m)
fromEvent (AnEvent i) = AnEvent
  $ dimap
      (map always)
      (\a -> unwrap (always (Endo (const a)) :: Endo Function (m (m Unit))) (pure (pure unit)))
      i

type Backdoor = { makeEvent :: MakeEvent
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
          subscribers <- liftST $ Ref.new []
          pure
            { event:
                AnEvent \k -> do
                  _ <- liftST $ Ref.modify (_ <> [ k ]) subscribers
                  pure do
                    _ <- liftST $ Ref.modify (deleteBy unsafeRefEq k) subscribers
                    pure unit
            , push:
                \a -> do
                  (liftST $ (Ref.read subscribers)) >>= traverse_ \k -> k a
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
          r <- liftST $ Ref.new Map.empty
          k1 $ f \a -> makeEvent \k2 -> do
            void $ liftST $ Ref.modify
              ( Map.alter
                  ( case _ of
                      Nothing -> Just [ k2 ]
                      Just arr -> Just (arr <> [ k2 ])
                  )
                  a
              )
              r
            pure $ void $ liftST $ Ref.modify
              ( Map.alter
                  ( case _ of
                      Nothing -> Nothing
                      Just arr -> Just (deleteBy unsafeRefEq k2 arr)
                  )
                  a
              )
              r
          unsub <- subscribe e \{ address, payload } -> do
            o <- liftST $ Ref.read r
            case Map.lookup address o of
              Nothing -> pure unit
              Just arr -> for_ arr (_ $ payload)
          pure do
            -- free references - helps gc?
            void $ liftST $ Ref.write (Map.empty) r
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