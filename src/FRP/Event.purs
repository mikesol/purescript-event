module FRP.Event
  ( Event
  , EventIO
  , EventIO'
  , Subscriber(..)
  , create
  , createO
  , delay
  , mailbox
  , mailbox'
  , makeEvent
  , makeEventE
  , merge
  , module Class
  , once
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
import Control.Monad.ST.Ref as STRef
import Control.Monad.ST.Uncurried (STFn1, STFn2, mkSTFn1, mkSTFn2, runSTFn1)
import Data.Array (deleteBy, length)
import Data.Array as Array
import Data.Array.ST as STArray
import Data.Compactable (class Compactable)
import Data.Either (Either(..), either, hush)
import Data.Filterable as Filterable
import Data.Foldable (class Foldable, for_)
import Data.Foldable as M
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Time.Duration (Milliseconds(..))
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Aff as Aff
import Effect.Timer (TimeoutId, setTimeout)
import Effect.Uncurried (EffectFn1, EffectFn2, mkEffectFn1, runEffectFn1, runEffectFn2)
import FRP.Event.Class (class Filterable, class IsEvent, count, filterMap, fix, fold, folded, gate, gateBy, keepLatest, mapAccum, sampleOnRight, sampleOnRight_, withLast) as Class
import FRP.Event.Class (class IsEvent)
import Foreign.Object.ST as FOST
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
newtype Event a = Event (STFn1 (EffectFn1 a Unit) Global (ST Global Unit))

instance functorEvent :: Functor Event where
  map f (Event e) = Event (mkSTFn1 (\k -> runSTFn1 e (mkEffectFn1 (\a -> runEffectFn1 k (f a)))))

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
    Event $ mkSTFn1 \k -> ado
      c1 <- runSTFn1 f k
      c2 <- runSTFn1 g k
      in
        do
          c1
          c2

instance isEvent :: IsEvent Event where
  fix = fix
  keepLatest = keepLatest
  sampleOnRight = sampleOnRight
  sampleOnLeft = sampleOnLeft
  once = once

-- | Merge together several events. This has the same functionality
-- | as `oneOf`, but it is faster and less prone to stack explosions.
merge :: forall f a. Foldable f => f (Event a) → Event a
merge f = Event $ mkSTFn1 \k -> do
  a <- STArray.new
  f # M.foldMap \(Event i) -> do
    u <- runSTFn1 i k
    void $ STArray.push u a
  pure do
    o <- STArray.freeze a
    runSTFn1 fastForeachThunk o

instance plusEvent :: Plus Event where
  empty = Event (mkSTFn1 \_ -> pure (pure unit))

instance applyEvent :: Apply Event where
  apply a b = biSampleOn a ((#) <$> b)

instance semigroupEvent :: (Semigroup a) => Semigroup (Event a) where
  append = lift2 append

-- | Create an `Event` which only fires when a predicate holds.
filter :: forall a b. (a -> Maybe b) -> Event a -> Event b
filter p (Event e) =
  Event
    ( mkSTFn1 \k ->
        runSTFn1 e
          ( mkEffectFn1 \a -> case p a of
              Just y -> runEffectFn1 k y
              Nothing -> pure unit
          )
    )

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

sampleOnLeft :: forall a b. Event a -> Event (a -> b) -> Event b
sampleOnLeft (Event e1) (Event e2) =
  Event $ mkSTFn1 \k -> do
    latest <- STRef.new Nothing
    c1 <-
      runSTFn1 e1 $ mkEffectFn1 \a -> do
        o <- liftST $ STRef.read latest
        for_ o (\f -> runEffectFn1 k (f a))
    c2 <-
      runSTFn1 e2 $ mkEffectFn1 \f -> do
        void $ liftST $ STRef.write (Just f) latest
    pure do
      c1
      c2

-- | Create an `Event` which samples the latest values from the first event
-- | at the times when the second event fires.
sampleOnRight :: forall a b. Event a -> Event (a -> b) -> Event b
sampleOnRight (Event e1) (Event e2) =
  Event $ mkSTFn1 \k -> do
    latest <- STRef.new Nothing
    c1 <-
      runSTFn1 e1 $ mkEffectFn1 \a -> do
        void $ liftST $ STRef.write (Just a) latest
    c2 <-
      runSTFn1 e2 $ mkEffectFn1 \f -> do
        o <- liftST $ STRef.read latest
        for_ o (\a -> runEffectFn1 k (f a))
    pure do
      c1
      c2

biSampleOn :: forall a b. Event a -> Event (a -> b) -> Event b
biSampleOn (Event e1) (Event e2) =
  Event $ mkSTFn1 \k -> do
    latest1 <- STRef.new []
    purge1 <- STRef.new true
    replay1 <- liftST STArray.new
    latest2 <- STRef.new []
    purge2 <- STRef.new true
    replay2 <- liftST STArray.new
    -- First we capture the immediately emitted events
    capturing <- STRef.new true
    c1 <-
      runSTFn1 e1 $ mkEffectFn1 \a -> do
        o <- liftST $ STRef.read capturing
        if o then void $ liftST $ STArray.push a replay1
        else do
          purge <- liftST $ STRef.read purge1
          void $ liftST $ STRef.write false purge1
          when purge $ launchAff_ do
            Aff.delay (Milliseconds 0.0)
            void $ liftST $ STRef.write true purge1
          void $ liftST $ STRef.modify (if purge then const [ a ] else flip Array.snoc a) latest1
          res <- liftST $ STRef.read latest2
          for_ res (\f -> runEffectFn1 k (f a))
    c2 <-
      runSTFn1 e2 $ mkEffectFn1 \f -> do
        o <- liftST $ STRef.read capturing
        if o then void $ liftST $ STArray.push f replay2
        else do
          purge <- liftST $ STRef.read purge2
          void $ liftST $ STRef.write false purge2
          when purge $ launchAff_ do
            Aff.delay (Milliseconds 0.0)
            void $ liftST $ STRef.write true purge2
          void $ liftST $ STRef.modify (if purge then const [ f ] else flip Array.snoc f) latest2
          res <- liftST $ STRef.read latest1
          for_ res (\a -> runEffectFn1 k (f a))
    -- And then we replay them according to the `Applicative Array` instance
    _ <- void $ STRef.write false capturing
    samples1 <- liftST $ STArray.freeze replay1
    samples2 <- liftST $ STArray.freeze replay2
    -- case samples1 of
    --   -- if there are no samples in samples1, we still want to write samples2
    --   [] -> void $ STRef.write (Array.last samples2) latest2
    --   _ -> runSTFn1 fastForeachST samples1 $ mkSTFn1 \a -> do
    --     -- We write the current values as we go through -- this would only matter for recursive events
    --     void $ STRef.write (Just a) latest1
    --     runSTFn1 fastForeachST samples2 $ mkSTFn1 \f -> do
    --       void $ STRef.write (Just f) latest2
    --       runSTFn1 (unsafeCoerce k) (f a)
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
  Event $ mkSTFn1 \k -> do
    cancelInner <- STRef.new (pure unit)
    cancelOuter <-
      runSTFn1 e $ mkEffectFn1 \(Event inner) -> liftST do
        -- in rare cases, cancelOuter may itself provoke an emission
        -- of the outer event, in which case this function would run
        -- to avoid that, we use a `safeToIgnore` flag
        ci <- STRef.read cancelInner
        ci
        c <- runSTFn1 inner k
        void $ STRef.write c cancelInner
    pure do
      ci <- STRef.read cancelInner
      ci
      cancelOuter

-- | Compute a fixed point
fix :: forall i. (Event i -> Event i) -> Event i
fix f =
  Event $ mkSTFn1 \k -> do
    { event, push } <- create'
    let Event e0 = f event
    let Event e1 = event
    c2 <- runSTFn1 e1 k
    c1 <- runSTFn1 e0 push
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
subscribe (Event e) k = runSTFn1 e (mkEffectFn1 k)

-- | Subscribe to an `Event` by providing a callback.
-- |
-- | `subscribe` returns a canceller function.
subscribeO
  :: forall a
   . STFn2 (Event a) (EffectFn1 a Unit) Global (ST Global Unit)
subscribeO = mkSTFn2 \(Event e) k -> runSTFn1 e k

subscribePureO
  :: forall a
   . STFn2 (Event a) (STFn1 a Global Unit) Global (ST Global Unit)
subscribePureO = mkSTFn2 \(Event e) k -> runSTFn1 e (stPusherToEffectPusher k)
  where

  stPusherToEffectPusher :: forall rr aa. (STFn1 a rr Unit) -> EffectFn1 aa Unit
  stPusherToEffectPusher = unsafeCoerce

subscribePure
  :: forall a
   . Event a
  -> (a -> ST Global Unit)
  -> ST Global (ST Global Unit)
subscribePure (Event e) k = runSTFn1 e (mkEffectFn1 (stPusherToEffectPusher k))
  where

  stPusherToEffectPusher :: forall rr aa. (aa -> ST rr Unit) -> aa -> Effect Unit
  stPusherToEffectPusher = unsafeCoerce

newtype Subscriber = Subscriber (forall b. STFn2 (Event b) (EffectFn1 b Unit) Global (ST Global Unit))


type EventIO a =
  { event :: Event a
  , push :: a -> Effect Unit
  }

-- | Create an event and a function which supplies a value to that event.
create :: forall a. ST Global (EventIO a)
create = create_

makeEvent
  :: forall a
   . ((a -> Effect Unit) -> ST Global (ST Global Unit))
  -> Event a
makeEvent e = Event $ mkSTFn1 \k -> do
  e (\a -> runEffectFn1 k a)

makeEventE :: forall a. ((a -> Effect Unit) -> Effect (Effect Unit)) -> Effect { event :: Event a, unsubscribe :: Effect Unit }
makeEventE e = do
  { event, push } <- liftST create
  unsubscribe <- e push
  pure { event, unsubscribe }

type EventIO' a =
  { event :: Event a
  , push :: EffectFn1 a Unit
  }

create' :: forall a. ST Global (EventIO' a)
create' = do
  subscribers <- FOST.new
  idx <- STRef.new 0
  pure
    { event:
        Event $ mkSTFn1 \k -> do
          rk <- STRef.new k
          ix <- STRef.read idx
          void $ FOST.poke (show ix) rk subscribers
          void $ STRef.modify (_ + 1) idx
          pure do
            void $ STRef.write mempty rk
            void $ FOST.delete (show ix) subscribers
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
  subscribers <- FOST.new
  idx <- STRef.new 0
  pure
    { event:
        Event $ mkSTFn1 \k -> do
          rk <- STRef.new k
          ix <- STRef.read idx
          void $ FOST.poke (show ix) rk subscribers
          void $ STRef.modify (_ + 1) idx
          pure do
            void $ STRef.write mempty rk
            void $ FOST.delete (show ix) subscribers
            pure unit
    , push:
        \a -> do
          runEffectFn2 fastForeachOhE subscribers $ mkEffectFn1 \rk -> do
            k <- liftST $ STRef.read rk
            runEffectFn1 k a
    }

createO
  :: forall a
   . ST Global (EventIO' a)
createO = create'

mailbox' :: forall a b. Ord a => ST Global { push :: EffectFn1 { address :: a, payload :: b } Unit, event :: a -> Event b }
mailbox' = do
  r <- STRef.new Map.empty
  pure
    { event: \a -> Event $ mkSTFn1 \k2 -> do
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

mailbox :: forall a b. Ord a => ST Global { push :: { address :: a, payload :: b } -> Effect Unit, event :: a -> Event b }
mailbox = do
  { push, event } <- mailbox'
  pure { event, push: \k -> runEffectFn1 push k }

--
foreign import fastForeachThunk :: STFn1 (Array (ST Global Unit)) Global Unit
foreign import fastForeachE :: forall a. EffectFn2 (Array a) (EffectFn1 a Unit) Unit
foreign import fastForeachST :: forall a. STFn2 (Array a) (STFn1 a Global Unit) Global Unit
foreign import fastForeachOhE :: forall a. EffectFn2 (FOST.STObject Global a) (EffectFn1 a Unit) Unit

--

delay :: forall a. Int -> Event a -> Event (Either TimeoutId (Tuple (Maybe TimeoutId) a))
delay n (Event e) = Event $ mkSTFn1 \k -> do
  runSTFn1 e $ mkEffectFn1 \a -> do
    tid <- liftST $ STRef.new Nothing
    o <- setTimeout n do
      t <- liftST $ STRef.read tid
      runEffectFn1 k (Right (Tuple t a))
    void $ liftST $ STRef.write (Just o) tid
    runEffectFn1 k (Left o)
