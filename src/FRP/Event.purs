module FRP.Event
  ( Event
  , EventIO
  , EventIO'
  , PureEventIO
  , PureEventIO'
  , Subscriber(..)
  , memoize
  , memoized
  , mailboxed
  , makeEventE
  , mailbox'
  , mailbox
  , merge
  , create
  , createO
  , createPure
  , createPureO
  , delay
  , hot
  , makeEvent
  , makeEventO
  , makeLemmingEvent
  , makeLemmingEventO
  , makePureEvent
  , module Class
  , subscribe
  , subscribeO
  , subscribePure
  , subscribePureO
  ) where

import Prelude

import Control.Alternative (class Alt, class Plus)
import Control.Apply (lift2)
import Control.Monad.ST (ST)
import Control.Monad.ST.Class (liftST)
import Control.Monad.ST.Global (Global)
import Control.Monad.ST.Internal as STRef
import Control.Monad.ST.Uncurried (STFn1, STFn2, STFn3, mkSTFn2, runSTFn1, runSTFn2, runSTFn3)
import Data.Array (deleteBy)
import Data.Array.ST as STArray
import Data.Compactable (class Compactable)
import Data.Either (Either(..), either, hush)
import Data.Filterable as Filterable
import Data.Foldable (class Foldable, for_)
import Data.Foldable as M
import Data.FunctorWithIndex (class FunctorWithIndex)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Timer (TimeoutId, setTimeout)
import Effect.Uncurried (EffectFn1, EffectFn2, mkEffectFn1, runEffectFn1, runEffectFn2)
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
  map f (Event e) = Event (mkSTFn2 (\b k -> runSTFn2 e b (mkEffectFn1 (\a -> runEffectFn1 k (f a)))))

instance functorWithIndexEvent :: FunctorWithIndex Int Event where
  mapWithIndex f e = Class.mapAccum (\a b -> Tuple (a + 1) (f a b)) 0 e

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
    Event $ mkSTFn2 \tf k -> ado
      c1 <- runSTFn2 f tf k
      c2 <- runSTFn2 g tf k
      in
        do
          c1
          c2

-- | Merge together several events. This has the same functionality
-- | as `oneOf`, but it is faster and less prone to stack explosions.
merge :: forall f a. Foldable f => f (Event a) → Event a
merge f = Event $ mkSTFn2 \tf k -> do
  a <- STArray.new
  f # M.foldMap \(Event i) -> do
    u <- runSTFn2 i tf k
    void $ liftST $ STArray.push u a
  pure do
    o <- liftST (STArray.freeze a)
    runSTFn1 fastForeachThunk o

instance plusEvent :: Plus Event where
  empty = Event (mkSTFn2 \_ _ -> pure (pure unit))

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

once :: forall a. Event a -> Event a
once (Event e) =
  Event $ mkSTFn2 \b k -> do
    latest <- STRef.new Nothing
    u <- STRef.new $ pure unit
    c <-
      runSTFn2 e b $ mkEffectFn1 \a -> do
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
    ( mkSTFn2 \tf k ->
        runSTFn2 e tf
          ( mkEffectFn1 \a -> case p a of
              Just y -> runEffectFn1 k y
              Nothing -> pure unit
          )
    )

sampleOnLeft :: forall a b. Event a -> Event (a -> b) -> Event b
sampleOnLeft (Event e1) (Event e2) =
  Event $ mkSTFn2 \b k -> do
    latest <- STRef.new Nothing
    c1 <-
      runSTFn2 e1 b $ mkEffectFn1 \a -> do
        o <- liftST $ STRef.read latest
        for_ o (\f -> runEffectFn1 k (f a))
    c2 <-
      runSTFn2 e2 b $ mkEffectFn1 \f -> do
        liftST $ void $ STRef.write (Just f) latest
    pure do
      c1
      c2

-- | Create an `Event` which samples the latest values from the first event
-- | at the times when the second event fires.
sampleOnRight :: forall a b. Event a -> Event (a -> b) -> Event b
sampleOnRight (Event e1) (Event e2) =
  Event $ mkSTFn2 \b k -> do
    latest <- STRef.new Nothing
    c1 <-
      runSTFn2 e1 b $ mkEffectFn1 \a -> do
        void $ liftST $ STRef.write (Just a) latest
    c2 <-
      runSTFn2 e2 b $ mkEffectFn1 \f -> do
        o <- liftST $ STRef.read latest
        for_ o (\a -> runEffectFn1 k (f a))
    pure do
      c1
      c2

biSampleOn :: forall a b. Event a -> Event (a -> b) -> Event b
biSampleOn (Event e1) (Event e2) =
  Event $ mkSTFn2 \tf k -> do
    latest1 <- STRef.new Nothing
    latest2 <- STRef.new Nothing
    c1 <-
      runSTFn2 e1 tf $ mkEffectFn1 \a -> do
        void $ liftST $ STRef.write (Just a) latest1
        res <- liftST $ STRef.read latest2
        for_ res (\f -> runEffectFn1 k (f a))
    c2 <-
      runSTFn2 e2 tf $ mkEffectFn1 \f -> do
        void $ liftST $ STRef.write (Just f) latest2
        res <- liftST $ STRef.read latest1
        for_ res (\a -> runEffectFn1 k (f a))
    pure do
      c1
      c2

-- | Flatten a nested `Event`, reporting values only from the most recent
-- | inner `Event`.
keepLatest :: forall a. Event (Event a) -> Event a
keepLatest (Event e) =
  Event $ mkSTFn2 \tf k -> do
    cancelInner <- STRef.new (pure unit)
    cancelOuter <-
      runSTFn2 e tf $ mkEffectFn1 \(Event inner) -> liftST do
        ci <- STRef.read cancelInner
        ci
        c <- runSTFn2 inner tf k
        void $ liftST $ STRef.write c cancelInner
    pure do
      ci <- STRef.read cancelInner
      ci
      cancelOuter

-- | Compute a fixed point
fix :: forall i. (Event i -> Event i) -> Event i
fix f =
  Event $ mkSTFn2 \tf k -> do
    { event, push } <- create'
    let Event e0 = f event
    let Event e1 = event
    c2 <- runSTFn2 e1 tf k
    c1 <- runSTFn2 e0 tf push
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
  -> ST Global (ST Global Unit)
subscribe (Event e) k = runSTFn2 e false (mkEffectFn1 k)

-- | Subscribe to an `Event` by providing a callback.
-- |
-- | `subscribe` returns a canceller function.
subscribeO
  :: forall a
   . STFn2 (Event a) (EffectFn1 a Unit) Global (ST Global Unit)
subscribeO = mkSTFn2 \(Event e) k -> runSTFn2 e false k

subscribePureO
  :: forall a
   . STFn2 (Event a) (STFn1 a Global Unit) Global (ST Global Unit)
subscribePureO = mkSTFn2 \(Event e) k -> (runSTFn2 e true (stPusherToEffectPusher k))
  where
  stPusherToEffectPusher :: forall aa. (STFn1 a Global Unit) -> EffectFn1 aa Unit
  stPusherToEffectPusher = unsafeCoerce

subscribePure
  :: forall a
   . Event a
  -> (a -> ST Global Unit)
  -> ST Global (ST Global Unit)
subscribePure (Event e) k = runSTFn2 e true (mkEffectFn1 (stPusherToEffectPusher k))
  where

  stPusherToEffectPusher :: forall aa. (aa -> ST Global Unit) -> aa -> Effect Unit
  stPusherToEffectPusher = unsafeCoerce

-- | Make an `Event` from a function which accepts a callback and returns an
-- | unsubscription function.
-- |
-- | Note: you probably want to use `create` instead, unless you need explicit
-- | control over unsubscription.
makeEvent
  :: forall a
   . ((a -> Effect Unit) -> ST Global (ST Global Unit))
  -> Event a
makeEvent e = Event $ mkSTFn2 \tf k ->
  if tf then pure (pure unit) else e (\a -> runEffectFn1 k a)

-- | Make a pure `Event` from a function which accepts a callback and returns an
-- | unsubscription function.
-- |
-- | Note: you probably want to use `create` instead, unless you need explicit
-- | control over unsubscription.
makePureEvent
  :: forall a
   . ((a -> ST Global Unit) -> ST Global (ST Global Unit))
  -> Event a
makePureEvent e = Event $ mkSTFn2 \_ k -> do
  let
    stEventToEvent :: forall aa. ((aa -> ST Global Unit) -> ST Global (ST Global Unit)) -> (aa -> Effect Unit) -> ST Global (ST Global Unit)
    stEventToEvent = unsafeCoerce
  stEventToEvent e (\a -> runEffectFn1 k a)

makeEventE :: forall a. ((a -> Effect Unit) -> Effect (Effect Unit)) -> Effect { event :: Event a, unsubscribe :: Effect Unit }
makeEventE e = do
  { event, push } <- liftST create
  unsubscribe <- e push
  pure { event, unsubscribe }

makeEventO
  :: forall a
   . STFn1 (EffectFn1 a Unit) Global (ST Global Unit)
  -> Event a
makeEventO e = Event $ mkSTFn2 \tf k ->
  if tf then pure (pure unit) else runSTFn1 e k

makeLemmingEvent
  :: forall a
   . ((forall b. Event b -> (b -> ST Global Unit) -> ST Global (ST Global Unit)) -> (a -> ST Global Unit) -> ST Global (ST Global Unit))
  -> Event a
makeLemmingEvent e = Event $ mkSTFn2 \tf k -> do
  let

    stPusherToEffectPusher :: forall aa. (aa -> ST Global Unit) -> aa -> Effect Unit
    stPusherToEffectPusher = unsafeCoerce

    stEventToEvent :: forall aa. ((aa -> ST Global Unit) -> ST Global (ST Global Unit)) -> (aa -> Effect Unit) -> ST Global (ST Global Unit)
    stEventToEvent = unsafeCoerce

    o :: forall aa. Event aa -> (aa -> ST Global Unit) -> ST Global (ST Global Unit)
    o (Event ev) kx = runSTFn2 ev tf (mkEffectFn1 (stPusherToEffectPusher kx))

  stEventToEvent (e o) (\a -> runEffectFn1 k a)

newtype Subscriber = Subscriber (forall b. STFn2 (Event b) (STFn1 b Global Unit) Global (ST Global Unit))

makeLemmingEventO
  :: forall a
   . STFn2 Subscriber (STFn1 a Global Unit) Global (ST Global Unit)
  -> Event a
makeLemmingEventO e = Event $ mkSTFn2 \tf k -> do
  let

    stPusherToEffectPusher :: forall aa. STFn1 aa Global Unit -> EffectFn1 aa Unit
    stPusherToEffectPusher = unsafeCoerce

    stEventToEvent :: forall aa. (STFn2 Subscriber (STFn1 aa Global Unit) Global (ST Global Unit)) -> STFn2 Subscriber (EffectFn1 aa Unit) Global (ST Global Unit)
    stEventToEvent = unsafeCoerce

    o :: forall aa. STFn2 (Event aa) (STFn1 aa Global Unit) Global (ST Global Unit)
    o = mkSTFn2 \(Event ev) kx -> runSTFn2 ev tf (stPusherToEffectPusher kx)

  runSTFn2 (stEventToEvent e) (Subscriber o) k

type EventIO a =
  { event :: Event a
  , push :: a -> Effect Unit
  }

-- | Create an event and a function which supplies a value to that event.
create :: forall a. ST Global (EventIO a)
create = create_

type EventIO' a =
  { event :: Event a
  , push :: EffectFn1 a Unit
  }

data ObjHack (a :: Type)

foreign import objHack :: forall a. ST Global (ObjHack a)
foreign import insertObjHack :: forall a. STFn3 Int a (ObjHack a) Global Unit
foreign import deleteObjHack :: forall a. STFn2 Int (ObjHack a) Global Unit

memoized :: forall r a. Event a -> (Event a -> r) -> Event r
memoized e f = makeEvent \k -> do
  { event, push } <- create
  done <- STRef.new false
  subscribe e \a -> do
    o <- liftST $ STRef.read done
    unless o $ k (f event)
    void $ liftST $ STRef.write true done
    push a

memoize :: forall a. Event a -> ST Global {event :: Event a, unsubscribe :: ST Global Unit }
memoize e = do
  { event, push } <- create
  unsubscribe <- subscribe e push
  pure { event, unsubscribe }

mailboxed :: forall r a b. Ord a => Event { address :: a, payload :: b } -> ((a -> Event b) -> r) -> Event r
mailboxed e f = makeEvent \k -> do
  { event, push } <- mailbox
  done <- STRef.new false
  subscribe e \a -> do
    o <- liftST $ STRef.read done
    unless o $ k (f event)
    void $ liftST $ STRef.write true done
    push a

create' :: forall a. ST Global (EventIO' a)
create' = do
  subscribers <- objHack
  idx <- STRef.new 0
  pure
    { event:
        Event $ mkSTFn2 \_ k -> do
          rk <- STRef.new k
          ix <- STRef.read idx
          runSTFn3 insertObjHack ix rk subscribers
          void $ STRef.modify (_ + 1) idx
          pure do
            void $ STRef.write mempty rk
            runSTFn2 deleteObjHack ix subscribers
            pure unit
    , push:
        mkEffectFn1 \a -> do
          runEffectFn2 fastForeachOhE subscribers $ mkEffectFn1 \rk -> do
            k <- liftST $ STRef.read rk
            runEffectFn1 k a
    }

create_
  :: forall a
   . ST Global (EventIO a)
create_ = do
  subscribers <- objHack
  idx <- STRef.new 0
  pure
    { event:
        Event $ mkSTFn2 \_ k -> do
          rk <- STRef.new k
          ix <- STRef.read idx
          runSTFn3 insertObjHack ix rk subscribers
          void $ STRef.modify (_ + 1) idx
          pure do
            void $ STRef.write mempty rk
            runSTFn2 deleteObjHack ix subscribers
            pure unit
    , push:
        \a -> do
          runEffectFn2 fastForeachOhE subscribers $ mkEffectFn1 \rk -> do
            k <- liftST $ STRef.read rk
            runEffectFn1 k a
    }

-- | Create an event and a function which supplies a value to that event in ST.
createPure
  :: forall a r
   . ST r (PureEventIO r a)
createPure = (unsafeCoerce :: ST Global (EventIO a) -> ST r (PureEventIO r a)) create_

type PureEventIO r a =
  { event :: Event a
  , push :: a -> ST r Unit
  }

type PureEventIO' r a =
  { event :: Event a
  , push :: STFn1 a r Unit
  }

createPureO
  :: forall a
   . ST Global (PureEventIO' Global a)
createPureO = (unsafeCoerce :: ST Global (EventIO' a) -> ST Global (PureEventIO' Global a)) create'

createO
  :: forall a
   . ST Global (EventIO' a)
createO = create'

mailbox :: forall a b. Ord a => ST Global { push :: { address :: a, payload :: b } -> Effect Unit, event :: a -> Event b }
mailbox = do
  { push, event } <- mailbox'
  pure
    { push: \ap -> runEffectFn1 push ap
    , event
    }

mailbox' :: forall a b. Ord a => ST Global { push :: EffectFn1 { address :: a, payload :: b } Unit, event :: a -> Event b }
mailbox' = do
  r <- STRef.new Map.empty
  pure
    { event: \a -> Event $ mkSTFn2 \_ k2 -> do
        void $ STRef.modify
          ( Map.alter
              ( case _ of
                  Nothing -> Just [ k2 ]
                  Just arr -> Just (arr <> [ k2 ])
              )
              a
          )
          r
        pure $ void $ STRef.modify
          ( Map.alter
              ( case _ of
                  Nothing -> Nothing
                  Just arr -> Just (deleteBy unsafeRefEq k2 arr)
              )
              a
          )
          r
    , push: mkEffectFn1 \{ address, payload } -> do
        o <- liftST $ STRef.read r
        case Map.lookup address o of
          Nothing -> pure unit
          Just arr -> runEffectFn2 fastForeachE arr $ mkEffectFn1 \i -> runEffectFn1 i payload
    }

-- | Makes an event hot, meaning that it will start firing on left-bind. This means that `pure` should never be used with `hot` as it will be lost. Use this for loops, for example.
hot
  :: forall a
   . Event a
  -> ST Global { event :: Event a, unsubscribe :: ST Global Unit }
hot e = do
  { event, push } <- create
  unsubscribe <- subscribe e push
  pure { event, unsubscribe }

--
foreign import fastForeachThunk :: STFn1 (Array (ST Global Unit)) Global Unit
foreign import fastForeachE :: forall a. EffectFn2 (Array a) (EffectFn1 a Unit) Unit
foreign import fastForeachOhE :: forall a. EffectFn2 (ObjHack a) (EffectFn1 a Unit) Unit

--

delay :: forall a. Int -> Event a -> Event (Either TimeoutId (Tuple (Maybe TimeoutId) a))
delay n (Event e) = Event $ mkSTFn2 \tf k -> do
  runSTFn2 e tf $ mkEffectFn1 \a -> do
    tid <- liftST $ STRef.new Nothing
    o <- setTimeout n do
      t <- liftST $ STRef.read tid
      runEffectFn1 k (Right (Tuple t a))
    void $ liftST $ STRef.write (Just o) tid
    runEffectFn1 k (Left o)