module FRP.Event
  ( Backdoor(..)
  , Bus(..)
  , BusT
  , Create(..)
  , CreateO(..)
  , CreateOT
  , CreatePure(..)
  , CreatePureO(..)
  , CreatePureOT
  , CreatePureT
  , CreateT
  , Delay(..)
  , DelayT
  , Event
  , EventIO
  , EventIO'
  , Hot(..)
  , HotT
  , Mailbox(..)
  , MailboxT
  , Mailboxed(..)
  , MailboxedT
  , MakeEvent(..)
  , MakeEventO(..)
  , MakeEventOT
  , MakeEventT
  , MakeLemmingEvent(..)
  , MakeLemmingEventO(..)
  , MakeLemmingEventOT
  , MakeLemmingEventT
  , MakePureEvent(..)
  , MakePureEventT
  , Memoize(..)
  , MemoizeT
  , PureEventIO
  , PureEventIO'
  , Subscribe(..)
  , SubscribeO(..)
  , SubscribeOT
  , SubscribePure(..)
  , SubscribePureO(..)
  , SubscribePureOT
  , SubscribePureT
  , SubscribeT
  , Subscriber(..)
  , backdoor
  , burning
  , bus
  , merge
  , create
  , createO
  , createPure
  , createPureO
  , delay
  , hot
  , mailbox
  , mailboxed
  , makeEvent
  , makeEventO
  , makeLemmingEvent
  , makeLemmingEventO
  , makePureEvent
  , memoize
  , module Class
  , subscribe
  , subscribeO
  , subscribePure
  , subscribePureO
  ) where

import Prelude

import Control.Alt ((<|>))
import Control.Alternative (class Alt, class Alternative, class Plus)
import Control.Apply (lift2)
import Control.Monad.ST (ST)
import Control.Monad.ST.Class (liftST)
import Control.Monad.ST.Uncurried (STFn1, STFn2, mkSTFn2)
import Data.Array (deleteBy, length)
import Data.Array as Array
import Data.Array.ST as STArray
import Data.Compactable (class Compactable)
import Data.Either (Either(..), either, hush)
import Data.Filterable as Filterable
import Data.Foldable (class Foldable, for_)
import Data.Foldable as M
import Data.HeytingAlgebra (ff, implies, tt)
import Data.Map as Map
import Data.Maybe (Maybe(..), maybe)
import Data.Set (Set, singleton, delete)
import Effect (Effect)
import Effect.Ref as ERef
import Effect.Ref as Ref
import Effect.Timer (TimeoutId, clearTimeout, setTimeout)
import Effect.Uncurried (EffectFn1, EffectFn2, EffectFn3, mkEffectFn1, mkEffectFn2, runEffectFn1, runEffectFn2, runEffectFn3)
import FRP.Event.Class (class Filterable, class IsEvent, count, filterMap, fix, fold, folded, gate, gateBy, keepLatest, mapAccum, sampleOnRight, sampleOnRight_, withLast) as Class
import Unsafe.Coerce (unsafeCoerce)
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
newtype Event a = Event (EffectFn2 Boolean (EffectFn1 a Unit) (Effect Unit))

-- boolean :: t = pure,false = impure

instance functorEvent :: Functor Event where
  map f (Event e) = Event (mkEffectFn2 (\b k -> runEffectFn2 e b (mkEffectFn1 (\a -> runEffectFn1 k (f a)))))

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
  alt (Event f) (Event g) =
    Event $ mkEffectFn2 \tf k -> ado
      c1 <- runEffectFn2 f tf k
      c2 <- runEffectFn2 g tf k
      in
        do
          c1
          c2

-- | Merge together several events. This has the same functionality
-- | as `oneOf`, but it is faster and less prone to stack explosions.
merge :: forall f a. Foldable f => f (Event a) → Event a
merge f = Event $ mkEffectFn2 \tf k -> do
  a <- liftST $ STArray.new
  f # M.foldMap \(Event i) -> do
    u <- runEffectFn2 i tf k
    void $ liftST $ STArray.push u a
  pure do
      o <- liftST (STArray.freeze a)
      runEffectFn1 fastForeachThunk o

instance plusEvent :: Plus Event where
  empty = Event (mkEffectFn2 \_ _ -> pure (pure unit))

instance applyEvent :: Apply Event where
  apply a b = biSampleOn a ((#) <$> b)

instance applicativeEvent :: Applicative Event where
  pure a = Event $ mkEffectFn2 \_ k -> do
    runEffectFn1 k a
    pure (pure unit)

instance alternativeEvent :: Alternative Event

instance eventIsEvent :: Class.IsEvent Event where
  keepLatest = keepLatest
  sampleOnRight = sampleOnRight
  sampleOnLeft = sampleOnLeft
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

-- | Create an `Event` which only fires when a predicate holds.
filter :: forall a b. (a -> Maybe b) -> Event a -> Event b
filter p (Event e) =
  Event
    ( mkEffectFn2 \tf k ->
        runEffectFn2 e tf
          ( mkEffectFn1 \a -> case p a of
              Just y -> runEffectFn1 k y
              Nothing -> pure unit
          )
    )

sampleOnLeft :: forall a b. Event a -> Event (a -> b) -> Event b
sampleOnLeft (Event e1) (Event e2) =
  Event $ mkEffectFn2 \b k -> do
    latest <- Ref.new Nothing
    c1 <-
      runEffectFn2 e1 b $ mkEffectFn1 \a -> do
        o <- Ref.read latest
        for_ o (\f -> runEffectFn1 k (f a))
    c2 <-
      runEffectFn2 e2 b $ mkEffectFn1 \f -> do
        Ref.write (Just f) latest
    pure do
      c1
      c2

-- | Create an `Event` which samples the latest values from the first event
-- | at the times when the second event fires.
sampleOnRight :: forall a b. Event a -> Event (a -> b) -> Event b
sampleOnRight (Event e1) (Event e2) =
  Event $ mkEffectFn2 \b k -> do
    latest <- Ref.new Nothing
    c1 <-
      runEffectFn2 e1 b $ mkEffectFn1 \a -> do
        Ref.write (Just a) latest
    c2 <-
      runEffectFn2 e2 b $ mkEffectFn1 \f -> do
        o <- Ref.read latest
        for_ o (\a -> runEffectFn1 k (f a))
    pure do
      c1
      c2

biSampleOn :: forall a b. Event a -> Event (a -> b) -> Event b
biSampleOn (Event e1) (Event e2) =
  Event $ mkEffectFn2 \tf k -> do
    latest1 <- Ref.new Nothing
    replay1 <- liftST STArray.new
    latest2 <- Ref.new Nothing
    replay2 <- liftST STArray.new
    -- First we capture the immediately emitted events
    capturing <- Ref.new true
    c1 <-
      runEffectFn2 e1 tf $ mkEffectFn1 \a -> do
        o <- Ref.read capturing
        if o then void $ liftST $ STArray.push a replay1
        else do
          Ref.write (Just a) latest1
          res <- Ref.read latest2
          for_ res (\f -> runEffectFn1 k (f a))
    c2 <-
      runEffectFn2 e2 tf $ mkEffectFn1 \f -> do
        o <- Ref.read capturing
        if o then void $ liftST $ STArray.push f replay2
        else do
          Ref.write (Just f) latest2
          res <- Ref.read latest1
          for_ res (\a -> runEffectFn1 k (f a))
    -- And then we replay them according to the `Applicative Array` instance
    _ <- Ref.write false capturing
    samples1 <- liftST $ STArray.freeze replay1
    samples2 <- liftST $ STArray.freeze replay2
    case samples1 of
      -- if there are no samples in samples1, we still want to write samples2
      [] -> Ref.write (Array.last samples2) latest2
      _ -> runEffectFn2 fastForeachE samples1 $ mkEffectFn1 \a -> do
        -- We write the current values as we go through -- this would only matter for recursive events
        Ref.write (Just a) latest1
        runEffectFn2 fastForeachE samples2 $ mkEffectFn1 \f -> do
          Ref.write (Just f) latest2
          runEffectFn1 k (f a)
    -- Free the samples so they can be GCed
    _ <- liftST $ STArray.splice 0 (length samples1) [] replay1
    _ <- liftST $ STArray.splice 0 (length samples2) [] replay2
    pure do
      c1
      c2

-- | Flatten a nested `Event`, reporting values only from the most recent
-- | inner `Event`.
keepLatest :: forall a. Event (Event a) -> Event a
keepLatest (Event e) =
  Event $ mkEffectFn2 \tf k -> do
    cancelInner <- Ref.new (pure unit)
    cancelOuter <-
      runEffectFn2 e tf $ mkEffectFn1 \(Event inner) -> do
        -- in rare cases, cancelOuter may itself provoke an emission
        -- of the outer event, in which case this function would run
        -- to avoid that, we use a `safeToIgnore` flag
        ci <- Ref.read cancelInner
        ci
        c <- runEffectFn2 inner tf k
        Ref.write c cancelInner
    pure do
      ci <- Ref.read cancelInner
      ci
      cancelOuter

-- | Compute a fixed point
fix :: forall i. (Event i -> Event i) -> Event i
fix f =
  Event $ mkEffectFn2 \tf k -> do
    { event, push } <- create'
    let Event e0 = f event
    let Event e1 = event
    c2 <- runEffectFn2 e1 tf k
    c1 <- runEffectFn2 e0 tf push
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

-- | Subscribe to an `Event` by providing a callback.
-- |
-- | `subscribe` returns a canceller function.
subscribeO :: SubscribeOT
subscribeO = (\(SubscribeO nt) -> nt) backdoor.subscribeO

type SubscribeOT =
  forall a
   . EffectFn2 (Event a) (EffectFn1 a Unit) (Effect Unit)

newtype SubscribeO = SubscribeO SubscribeOT

subscribePureO :: SubscribePureOT
subscribePureO = (\(SubscribePureO nt) -> nt) backdoor.subscribePureO

type SubscribePureOT =
  forall a r
   . STFn2 (Event a) (STFn1 a r Unit) r (ST r Unit)

newtype SubscribePureO = SubscribePureO SubscribePureOT

subscribePure :: SubscribePureT
subscribePure i = (\(SubscribePure nt) -> nt) backdoor.subscribePure i

type SubscribePureT =
  forall r a
   . Event a
  -> (a -> ST r Unit)
  -> ST r (ST r Unit)

newtype SubscribePure = SubscribePure SubscribePureT

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

--
type MakePureEventT =
  forall a r
   . ((a -> ST r Unit) -> ST r (ST r Unit))
  -> Event a

newtype MakePureEvent = MakePureEvent MakePureEventT

-- | Make a pure `Event` from a function which accepts a callback and returns an
-- | unsubscription function.
-- |
-- | Note: you probably want to use `create` instead, unless you need explicit
-- | control over unsubscription.
makePureEvent :: MakePureEventT
makePureEvent i = (\(MakePureEvent nt) -> nt) backdoor.makePureEvent i

--
type MakeEventOT =
  forall a
   . EffectFn1 (EffectFn1 a Unit) (Effect Unit)
  -> Event a

newtype MakeEventO = MakeEventO MakeEventOT

makeEventO :: MakeEventOT
makeEventO i = (\(MakeEventO nt) -> nt) backdoor.makeEventO i

--
type MakeLemmingEventT =
  forall a r
   . ((forall b. Event b -> (b -> ST r Unit) -> ST r (ST r Unit)) -> (a -> ST r Unit) -> ST r (ST r Unit))
  -> Event a

newtype MakeLemmingEvent = MakeLemmingEvent MakeLemmingEventT

makeLemmingEvent :: MakeLemmingEventT
makeLemmingEvent i = (\(MakeLemmingEvent nt) -> nt) backdoor.makeLemmingEvent i

newtype Subscriber = Subscriber (forall b r. STFn2 (Event b) (STFn1 b r Unit) r (ST r Unit))

type MakeLemmingEventOT =
  forall a r
   . STFn2 Subscriber (STFn1 a r Unit) r (ST r Unit)
  -> Event a

newtype MakeLemmingEventO = MakeLemmingEventO MakeLemmingEventOT

makeLemmingEventO :: MakeLemmingEventOT
makeLemmingEventO i = (\(MakeLemmingEventO nt) -> nt) backdoor.makeLemmingEventO i

type EventIO a =
  { event :: Event a
  , push :: a -> Effect Unit
  }

-- | Create an event and a function which supplies a value to that event.
create :: CreateT
create = do
  pure unit
  (\(Create nt) -> nt) backdoor.create

type EventIO' a =
  { event :: Event a
  , push :: EffectFn1 a Unit
  }

data ObjHack (a :: Type)

foreign import objHack :: forall a. Effect (ObjHack a)
foreign import insertObjHack :: forall a. EffectFn3 Int a (ObjHack a) Unit
foreign import deleteObjHack :: forall a. EffectFn2 Int (ObjHack a) Unit

create' :: forall a. Effect (EventIO' a)
create' = do
  subscribers <- objHack
  idx <- Ref.new 0
  pure
    { event:
        Event $ mkEffectFn2 \_ k -> do
          rk <- Ref.new k
          ix <- Ref.read idx
          runEffectFn3 insertObjHack ix rk subscribers
          Ref.modify_ (_ + 1) idx
          pure do
            Ref.write mempty rk
            runEffectFn2 deleteObjHack ix subscribers
            pure unit
    , push:
        mkEffectFn1 \a -> do
          runEffectFn2 fastForeachOhE subscribers $ mkEffectFn1 \rk -> do
            k <- Ref.read rk
            runEffectFn1 k a
    }

type CreateT =
  forall a
   . Effect (EventIO a)

newtype Create = Create CreateT

-- | Create an event and a function which supplies a value to that event in ST.
createPure :: CreatePureT
createPure = do
  pure unit
  (\(CreatePure nt) -> nt) backdoor.createPure

type PureEventIO r a =
  { event :: Event a
  , push :: a -> ST r Unit
  }

type PureEventIO' r a =
  { event :: Event a
  , push :: STFn1 a r Unit
  }

type CreatePureT =
  forall a r
   . ST r (PureEventIO r a)

newtype CreatePure = CreatePure CreatePureT

type CreatePureOT =
  forall a r
   . ST r (PureEventIO' r a)

newtype CreatePureO = CreatePureO CreatePureOT

createPureO :: CreatePureOT
createPureO = do
  pure unit
  (\(CreatePureO nt) -> nt) backdoor.createPureO

type CreateOT =
  forall a
   . Effect (EventIO' a)

newtype CreateO = CreateO CreateOT

createO :: CreateOT
createO = do
  pure unit
  (\(CreateO nt) -> nt) backdoor.createO

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

mailbox' :: forall a b. Ord a => Effect { push :: EffectFn1 { address :: a, payload :: b } Unit, event :: a -> Event b }
mailbox' = do
  r <- Ref.new Map.empty
  pure
    { event: \a -> Event $ mkEffectFn2 \_ k2 -> do
        void $ Ref.modify
          ( Map.alter
              ( case _ of
                  Nothing -> Just [ k2 ]
                  Just arr -> Just (arr <> [ k2 ])
              )
              a
          )
          r
        pure $ void $ Ref.modify
          ( Map.alter
              ( case _ of
                  Nothing -> Nothing
                  Just arr -> Just (deleteBy unsafeRefEq k2 arr)
              )
              a
          )
          r
    , push: mkEffectFn1 \{ address, payload } -> do
        o <- Ref.read r
        case Map.lookup address o of
          Nothing -> pure unit
          Just arr -> runEffectFn2 fastForeachE arr $ mkEffectFn1 \i -> runEffectFn1 i payload
    }

-- like mailbox, but in effect
mailbox :: MailboxT
mailbox = do
  pure unit
  (\(Mailbox nt) -> nt) backdoor.mailbox

type MailboxT = forall a b. Ord a => Effect { push :: { address :: a, payload :: b } -> Effect Unit, event :: a -> Event b }

newtype Mailbox = Mailbox MailboxT

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
burning i (Event e) = do
  r <- Ref.new i
  { event, push } <- create'
  unsubscribe <- runEffectFn2 e true $ mkEffectFn1 \x -> do
    Ref.write x r
    runEffectFn1 push x
  pure
    { event: event <|>
        ( Event $ mkEffectFn2 \_ k -> do
            o <- Ref.read r
            runEffectFn1 k o
            pure (pure unit)
        )
    , unsubscribe
    }

--
foreign import fastForeachThunk :: EffectFn1 (Array (Effect Unit)) Unit
foreign import fastForeachE :: forall a. EffectFn2 (Array a) (EffectFn1 a Unit) Unit
foreign import fastForeachOhE :: forall a. EffectFn2 (ObjHack a) (EffectFn1 a Unit) Unit

--

delay :: DelayT
delay i = (\(Delay nt) -> nt) backdoor.delay i

type DelayT = forall a. Int -> Event a -> Event a
newtype Delay = Delay DelayT

type Backdoor =
  { makeEvent :: MakeEvent
  , makeEventO :: MakeEventO
  , makePureEvent :: MakePureEvent
  , makeLemmingEvent :: MakeLemmingEvent
  , makeLemmingEventO :: MakeLemmingEventO
  , create :: Create
  , createO :: CreateO
  , createPure :: CreatePure
  , createPureO :: CreatePureO
  , subscribe :: Subscribe
  , subscribeO :: SubscribeO
  , subscribePure :: SubscribePure
  , subscribePureO :: SubscribePureO
  , bus :: Bus
  , memoize :: Memoize
  , hot :: Hot
  , mailboxed :: Mailboxed
  , mailbox :: Mailbox
  , delay :: Delay
  }

backdoor :: Backdoor
backdoor = do
  let
    create_ :: Create
    create_ = Create do
      subscribers <- objHack
      idx <- Ref.new 0
      pure
        { event:
            Event $ mkEffectFn2 \_ k -> do
              rk <- Ref.new k
              ix <- Ref.read idx
              runEffectFn3 insertObjHack ix rk subscribers
              Ref.modify_ (_ + 1) idx
              pure do
                Ref.write mempty rk
                runEffectFn2 deleteObjHack ix subscribers
                pure unit
        , push:
            \a -> do
              runEffectFn2 fastForeachOhE subscribers $ mkEffectFn1 \rk -> do
                k <- Ref.read rk
                runEffectFn1 k a
        }
  { createO: CreateO create'
  , makeEvent:
      let
        makeEvent_ :: MakeEvent
        makeEvent_ = MakeEvent
          \e -> Event $ mkEffectFn2 \tf k ->
            if tf then pure (pure unit) else e (\a -> runEffectFn1 k a)
      in
        makeEvent_
  , makeEventO:
      let
        makeEventO_ :: MakeEventO
        makeEventO_ = MakeEventO
          \e -> Event $ mkEffectFn2 \tf k ->
            if tf then pure (pure unit) else runEffectFn1 e k
      in
        makeEventO_
  , makePureEvent:
      let
        makePureEvent_ :: MakePureEvent
        makePureEvent_ = MakePureEvent
          \e -> Event $ mkEffectFn2 \_ k -> do
            let
              stEventToEvent :: forall r a. ((a -> ST r Unit) -> ST r (ST r Unit)) -> (a -> Effect Unit) -> Effect (Effect Unit)
              stEventToEvent = unsafeCoerce
            stEventToEvent e (\a -> runEffectFn1 k a)
      in
        makePureEvent_
  , makeLemmingEvent:
      let
        makeLemmingEvent_ :: MakeLemmingEvent
        makeLemmingEvent_ = MakeLemmingEvent
          \e -> Event $ mkEffectFn2 \tf k -> do
            let
              effectfulUnsubscribeToSTUnsubscribe :: forall r. Effect (Effect Unit) -> ST r (ST r Unit)
              effectfulUnsubscribeToSTUnsubscribe = unsafeCoerce

              stPusherToEffectPusher :: forall r a. (a -> ST r Unit) -> a -> Effect Unit
              stPusherToEffectPusher = unsafeCoerce

              stEventToEvent :: forall r a. ((a -> ST r Unit) -> ST r (ST r Unit)) -> (a -> Effect Unit) -> Effect (Effect Unit)
              stEventToEvent = unsafeCoerce

              o :: forall r a. Event a -> (a -> ST r Unit) -> ST r (ST r Unit)
              o (Event ev) kx = effectfulUnsubscribeToSTUnsubscribe $ runEffectFn2 ev tf (mkEffectFn1 (stPusherToEffectPusher kx))

            stEventToEvent (e o) (\a -> runEffectFn1 k a)
      in
        makeLemmingEvent_
  , makeLemmingEventO:
      let
        makeLemmingEventO_ :: MakeLemmingEventO
        makeLemmingEventO_ = MakeLemmingEventO
          \e -> Event $ mkEffectFn2 \tf k -> do
            let
              effectfulUnsubscribeToSTUnsubscribe :: forall r. Effect (Effect Unit) -> ST r (ST r Unit)
              effectfulUnsubscribeToSTUnsubscribe = unsafeCoerce

              stPusherToEffectPusher :: forall r a. STFn1 a r Unit -> EffectFn1 a Unit
              stPusherToEffectPusher = unsafeCoerce

              stEventToEvent :: forall r a. (STFn2 Subscriber (STFn1 a r Unit) r (ST r Unit)) -> EffectFn2 Subscriber (EffectFn1 a Unit) (Effect Unit)
              stEventToEvent = unsafeCoerce

              o :: forall r a. STFn2 (Event a) (STFn1 a r Unit) r (ST r Unit)
              o = mkSTFn2 \(Event ev) kx -> effectfulUnsubscribeToSTUnsubscribe $ runEffectFn2 ev tf (stPusherToEffectPusher kx)

            runEffectFn2 (stEventToEvent e) (Subscriber o) k
      in
        makeLemmingEventO_
  , create: create_
  , createPure: (unsafeCoerce :: Create -> CreatePure) create_
  , createPureO: (unsafeCoerce :: CreateO -> CreatePureO) (CreateO create')
  , subscribe:
      let
        subscribe_ :: Subscribe
        subscribe_ = Subscribe \(Event e) k -> runEffectFn2 e false (mkEffectFn1 k)
      in
        subscribe_
  , subscribeO:
      let
        subscribeO_ :: SubscribeO
        subscribeO_ = SubscribeO (mkEffectFn2 \(Event e) k -> runEffectFn2 e false k)
      in
        subscribeO_
  , subscribePureO:
      let
        subscribePureO_ :: SubscribePureO
        subscribePureO_ = SubscribePureO (mkSTFn2 \(Event e) k -> effectfulUnsubscribeToSTUnsubscribe (runEffectFn2 e true (stPusherToEffectPusher k)))
          where
          effectfulUnsubscribeToSTUnsubscribe :: forall r. Effect (Effect Unit) -> ST r (ST r Unit)
          effectfulUnsubscribeToSTUnsubscribe = unsafeCoerce

          stPusherToEffectPusher :: forall r a. (STFn1 a r Unit) -> EffectFn1 a Unit
          stPusherToEffectPusher = unsafeCoerce

      in
        subscribePureO_
  , subscribePure:
      let
        subscribePure_ :: SubscribePure
        subscribePure_ = SubscribePure o
          where
          effectfulUnsubscribeToSTUnsubscribe :: forall r. Effect (Effect Unit) -> ST r (ST r Unit)
          effectfulUnsubscribeToSTUnsubscribe = unsafeCoerce

          stPusherToEffectPusher :: forall r a. (a -> ST r Unit) -> a -> Effect Unit
          stPusherToEffectPusher = unsafeCoerce

          o :: forall r a. Event a -> (a -> ST r Unit) -> ST r (ST r Unit)
          o (Event e) k = effectfulUnsubscribeToSTUnsubscribe (runEffectFn2 e true (mkEffectFn1 (stPusherToEffectPusher k)))
      in
        subscribePure_
  , bus:
      let
        bus_ :: Bus
        bus_ = Bus \f -> Event $ mkEffectFn2 \_ k -> do
          { push, event } <- create
          runEffectFn1 k (f push event)
          pure (pure unit)
      in
        bus_
  , memoize:
      let
        memoize_ :: Memoize
        memoize_ = Memoize \(Event e) f -> Event $ mkEffectFn2 \b k -> do
          { push, event } <- create'
          runEffectFn1 k (f event)
          runEffectFn2 e b push
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
  , mailbox:
      let
        mailbox_ :: Mailbox
        mailbox_ = Mailbox do
          { push, event } <- mailbox'
          pure { event, push: \k -> runEffectFn1 push k }
      in
        mailbox_
  , mailboxed:
      let
        mailboxed_ :: Mailboxed
        mailboxed_ = Mailboxed \(Event e) f -> Event $ mkEffectFn2 \b k -> do
          { push, event } <- mailbox'
          runEffectFn1 k (f event)
          runEffectFn2 e b push
      in
        mailboxed_
  , delay:
      let
        delay_ :: Delay
        delay_ = Delay \n (Event e) ->
          Event $ mkEffectFn2 \tf k -> do
            tid <- ERef.new (mempty :: Set TimeoutId)
            canceler <-
              runEffectFn2 e tf $ mkEffectFn1 \a -> do
                localId <- ERef.new Nothing
                id <-
                  setTimeout n do
                    runEffectFn1 k a
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
