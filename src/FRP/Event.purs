module FRP.Event
  ( Event
  , EventIO
  , EventIO'
  , PureEventIO
  , PureEventIO'
  , Subscriber(..)
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
import Control.Monad.ST.Global (Global)
import Control.Monad.ST.Internal as STRef
import Control.Monad.ST.Uncurried (STFn1, STFn2, mkSTFn1, mkSTFn2, runSTFn1)
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
newtype Event a = Event (STFn2 Boolean (EffectFn1 a Unit) Global (ST Global Unit))

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

instance eventIsEvent :: Class.IsEvent Event where
  keepLatest = keepLatest
  sampleOnRight = sampleOnRight
  sampleOnLeft = sampleOnLeft
  fix = fix
  once = once

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

once :: forall a. Event a -> Event a
once (Event e) =
  Event $ mkSTFn1 \k -> do
    latest <- STRef.new Nothing
    u <- STRef.new $ pure unit
    c <-
      runSTFn1 e $ mkEffectFn1 \a -> do
        o <- liftST $ STRef.read latest
        case o of
          Nothing -> do
            void $ liftST $ STRef.write (Just a) latest
            runEffectFn1 k a
            liftST $ join (STRef.read u)
          -- should not hit here
          Just _ -> pure unit
    void $ STRef.write c u
    o <- liftST $ STRef.read latest
    case o of
      Just _ -> c
      _ -> pure unit
    pure do
      c

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
subscribe
  :: forall a
   . Event a
  -> (a -> Effect Unit)
  -> Effect (Effect Unit)
subscribe (Event e) k = runEffectFn2 e false (mkEffectFn1 k)

-- | Subscribe to an `Event` by providing a callback.
-- |
-- | `subscribe` returns a canceller function.
subscribeO
  :: forall a
   . EffectFn2 (Event a) (EffectFn1 a Unit) (Effect Unit)
subscribeO = mkEffectFn2 \(Event e) k -> runEffectFn2 e false k

subscribePureO
  :: forall a r
   . STFn2 (Event a) (STFn1 a r Unit) r (ST r Unit)
subscribePureO = mkSTFn2 \(Event e) k -> effectfulUnsubscribeToSTUnsubscribe (runSTFn2 e true (stPusherToEffectPusher k))
  where
  effectfulUnsubscribeToSTUnsubscribe :: forall rr. Effect (Effect Unit) -> ST rr (ST rr Unit)
  effectfulUnsubscribeToSTUnsubscribe = unsafeCoerce

  stPusherToEffectPusher :: forall rr aa. (STFn1 a rr Unit) -> EffectFn1 aa Unit
  stPusherToEffectPusher = unsafeCoerce

subscribePure
  :: forall r a
   . Event a
  -> (a -> ST r Unit)
  -> ST r (ST r Unit)
subscribePure (Event e) k = effectfulUnsubscribeToSTUnsubscribe (runEffectFn2 e true (mkEffectFn1 (stPusherToEffectPusher k)))
  where
  effectfulUnsubscribeToSTUnsubscribe :: forall rr. Effect (Effect Unit) -> ST rr (ST rr Unit)
  effectfulUnsubscribeToSTUnsubscribe = unsafeCoerce

  stPusherToEffectPusher :: forall rr aa. (aa -> ST rr Unit) -> aa -> Effect Unit
  stPusherToEffectPusher = unsafeCoerce

-- | Make an `Event` from a function which accepts a callback and returns an
-- | unsubscription function.
-- |
-- | Note: you probably want to use `create` instead, unless you need explicit
-- | control over unsubscription.
makeEvent
  :: forall a
   . ((a -> Effect Unit) -> Effect (Effect Unit))
  -> Event a
makeEvent e = Event $ mkEffectFn2 \tf k ->
  if tf then pure (pure unit) else e (\a -> runEffectFn1 k a)

-- | Make a pure `Event` from a function which accepts a callback and returns an
-- | unsubscription function.
-- |
-- | Note: you probably want to use `create` instead, unless you need explicit
-- | control over unsubscription.
makePureEvent
  :: forall a r
   . ((a -> ST r Unit) -> ST r (ST r Unit))
  -> Event a
makePureEvent e = Event $ mkEffectFn2 \_ k -> do
  let
    stEventToEvent :: forall rr aa. ((aa -> ST rr Unit) -> ST rr (ST rr Unit)) -> (aa -> Effect Unit) -> Effect (Effect Unit)
    stEventToEvent = unsafeCoerce
  stEventToEvent e (\a -> runEffectFn1 k a)

makeEventO
  :: forall a
   . EffectFn1 (EffectFn1 a Unit) (Effect Unit)
  -> Event a
makeEventO e = Event $ mkEffectFn2 \tf k ->
  if tf then pure (pure unit) else runEffectFn1 e k

makeLemmingEvent
  :: forall a r
   . ((forall b. Event b -> (b -> ST r Unit) -> ST r (ST r Unit)) -> (a -> ST r Unit) -> ST r (ST r Unit))
  -> Event a
makeLemmingEvent e = Event $ mkEffectFn2 \tf k -> do
  let
    effectfulUnsubscribeToSTUnsubscribe :: forall rr. Effect (Effect Unit) -> ST rr (ST rr Unit)
    effectfulUnsubscribeToSTUnsubscribe = unsafeCoerce

    stPusherToEffectPusher :: forall rr aa. (aa -> ST rr Unit) -> aa -> Effect Unit
    stPusherToEffectPusher = unsafeCoerce

    stEventToEvent :: forall rr aa. ((aa -> ST rr Unit) -> ST rr (ST rr Unit)) -> (aa -> Effect Unit) -> Effect (Effect Unit)
    stEventToEvent = unsafeCoerce

    o :: forall rr aa. Event aa -> (aa -> ST rr Unit) -> ST rr (ST rr Unit)
    o (Event ev) kx = effectfulUnsubscribeToSTUnsubscribe $ runEffectFn2 ev tf (mkEffectFn1 (stPusherToEffectPusher kx))

  stEventToEvent (e o) (\a -> runEffectFn1 k a)

newtype Subscriber = Subscriber (forall b r. STFn2 (Event b) (STFn1 b r Unit) r (ST r Unit))

makeLemmingEventO
  :: forall a r
   . STFn2 Subscriber (STFn1 a r Unit) r (ST r Unit)
  -> Event a
makeLemmingEventO e = Event $ mkEffectFn2 \tf k -> do
  let
    effectfulUnsubscribeToSTUnsubscribe :: forall rr. Effect (Effect Unit) -> ST rr (ST rr Unit)
    effectfulUnsubscribeToSTUnsubscribe = unsafeCoerce

    stPusherToEffectPusher :: forall rr aa. STFn1 aa rr Unit -> EffectFn1 aa Unit
    stPusherToEffectPusher = unsafeCoerce

    stEventToEvent :: forall rr aa. (STFn2 Subscriber (STFn1 aa rr Unit) rr (ST rr Unit)) -> EffectFn2 Subscriber (EffectFn1 aa Unit) (Effect Unit)
    stEventToEvent = unsafeCoerce

    o :: forall rr aa. STFn2 (Event aa) (STFn1 aa rr Unit) rr (ST rr Unit)
    o = mkSTFn2 \(Event ev) kx -> effectfulUnsubscribeToSTUnsubscribe $ runEffectFn2 ev tf (stPusherToEffectPusher kx)

  runEffectFn2 (stEventToEvent e) (Subscriber o) k

type EventIO a =
  { event :: Event a
  , push :: a -> Effect Unit
  }

-- | Create an event and a function which supplies a value to that event.
create :: forall a. Effect (EventIO a)
create = create_

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

create_
  :: forall a
   . Effect (EventIO a)
create_ = do
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

-- | Create an event and a function which supplies a value to that event in ST.
createPure
  :: forall a r
   . ST r (PureEventIO r a)
createPure = (unsafeCoerce :: Effect (EventIO a) -> ST r (PureEventIO r a)) create_

type PureEventIO r a =
  { event :: Event a
  , push :: a -> ST r Unit
  }

type PureEventIO' r a =
  { event :: Event a
  , push :: STFn1 a r Unit
  }

createPureO
  :: forall a r
   . ST r (PureEventIO' r a)
createPureO = (unsafeCoerce :: Effect (EventIO' a) -> ST r (PureEventIO' r a)) create'

createO
  :: forall a
   . Effect (EventIO' a)
createO = create'

-- | Creates an event bus within a closure.
bus :: forall r a. ((a -> Effect Unit) -> Event a -> r) -> Event r
bus f = Event $ mkEffectFn2 \_ k -> do
  { push, event } <- create
  runEffectFn1 k (f push event)
  pure (pure unit)

-- | Takes the entire domain of a and allows for ad-hoc specialization.
mailboxed :: forall r a b. Ord a => Event { address :: a, payload :: b } -> ((a -> Event b) -> r) -> Event r
mailboxed (Event e) f = Event $ mkEffectFn2 \b k -> do
  { push, event } <- mailbox'
  runEffectFn1 k (f event)
  runEffectFn2 e b push

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

mailbox :: forall a b. Ord a => Effect { push :: { address :: a, payload :: b } -> Effect Unit, event :: a -> Event b }
mailbox = do
  { push, event } <- mailbox'
  pure { event, push: \k -> runEffectFn1 push k }

-- | Takes an event and memoizes it within a closure.
-- | All interactions with the event in the closure will not trigger a fresh
-- | subscription. Outside the closure does, however, trigger a fresh subscription.
memoize :: forall r a. Event a -> (Event a -> r) -> Event r
memoize (Event e) f = Event $ mkEffectFn2 \b k -> do
  { push, event } <- create'
  runEffectFn1 k (f event)
  runEffectFn2 e b push

-- | Makes an event hot, meaning that it will start firing on left-bind. This means that `pure` should never be used with `hot` as it will be lost. Use this for loops, for example.
hot
  :: forall a
   . Event a
  -> Effect { event :: Event a, unsubscribe :: Effect Unit }
hot e = do
  { event, push } <- create
  unsubscribe <- subscribe e push
  pure { event, unsubscribe }

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

delay :: forall a. Int -> Event a -> Event a
delay n (Event e) = Event $ mkEffectFn2 \tf k -> do
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
