module Hyrule.Zora
  ( Zora
  , liftImpure
  , liftImpureMaybe
  , liftImpureOr
  , liftPure
  , runImpure
  , runPure
  )
  where

import Prelude

import Control.Apply (lift2)
import Control.Monad.Rec.Class (class MonadRec, Step(..), tailRecM)
import Control.Monad.ST (ST)
import Control.Monad.ST.Class (class MonadST)
import Control.Monad.ST.Global (Global, toEffect)
import Data.Function.Uncurried (Fn2, Fn4, mkFn2, mkFn4, runFn2, runFn4)
import Data.Maybe (Maybe(..))
import Effect (Effect)

-- | A monad for omitting impure computations.
newtype Zora :: Type -> Type
newtype Zora a = Zora
  ( forall k. Fn4 (Fn2 (Unit -> k) (Effect (Unit -> k)) k) (ST Global (Unit -> k) -> k) ((Unit -> k) -> k) (a -> k) k
  )

instance Functor Zora where
  map f (Zora m) = Zora
    ( mkFn4 \lfEf lfSt more done -> runFn4 m lfEf lfSt more \a -> done (f a)
    )

instance Apply Zora where
  apply (Zora mf) (Zora ma) = Zora
    ( mkFn4 \lfEf lfSt more done ->
        more \_ ->
          runFn4 mf lfEf lfSt more \f ->
            more \_ ->
              runFn4 ma lfEf lfSt more \a ->
                done (f a)
    )

instance Applicative Zora where
  pure a = Zora
    ( mkFn4 \_ _ _ done -> done a
    )

instance Bind Zora where
  bind (Zora m) f = Zora
    ( mkFn4 \lfEf lfSt more done ->
        more \_ ->
          runFn4 m lfEf lfSt more \a ->
            case f a of
              Zora n ->
                more \_ ->
                  runFn4 n lfEf lfSt more \b ->
                    done b
    )

instance Monad Zora

instance Semigroup a => Semigroup (Zora a) where
  append = lift2 append

instance Monoid a => Monoid (Zora a) where
  mempty = pure mempty

instance MonadST Global Zora where
  liftST = liftPure

instance MonadRec Zora where
  tailRecM f a = Zora
    ( mkFn4 \lfEf lfSt more done ->
        let
          loop = mkFn2 \current gas ->
            case f current of
              Zora m ->
                runFn4 m lfEf lfSt more \r ->
                  case r of
                    Loop v ->
                      if gas == 0 then
                        more \_ ->
                          runFn2 loop v 50
                      else
                        runFn2 loop v (gas - 1)
                    Done v ->
                      done v
        in
          runFn2 loop a 50
    )

data RunZora a
  = More (Unit -> RunZora a)
  | LiftEffect (Unit -> RunZora a) (Effect (Unit -> RunZora a))
  | LiftST (ST Global (Unit -> RunZora a))
  | Stop a

-- | Lift an `Effect` to `Zora`, returning `mempty` when omitted.
liftImpure :: forall a. Monoid a => Effect a -> Zora a
liftImpure m = Zora
  ( mkFn4 \lfEf _ _ done ->
      runFn2 lfEf (\_ -> done mempty) (map (\a _ -> done a) m)
  )

-- | Lift an `Effect` to `Zora`, returning an `a` when omitted.
liftImpureOr :: forall a. (Unit -> a) -> Effect a -> Zora a
liftImpureOr f m = Zora
  ( mkFn4 \lfEf _ _ done ->
      runFn2 lfEf (\_ -> done (f unit)) (map (\a _ -> done a) m)
  )

-- | Lift an `Effect` to `Zora`, returning `Nothing` when omitted.
liftImpureMaybe :: forall a. Effect a -> Zora (Maybe a)
liftImpureMaybe m = Zora
  ( mkFn4 \lfEf _ _ done ->
      runFn2 lfEf (\_ -> done Nothing) (map (\a _ -> done (Just a)) m)
  )

-- | Lift an `ST Global` to `Zora`.
liftPure :: forall a. ST Global a -> Zora a
liftPure m = Zora
  ( mkFn4 \_ lfSt _ done ->
      lfSt (map (\a _ -> done a) m)
  )

-- | Interpret `Zora` as an `Effect`.
runImpure :: forall a. Zora a -> Effect a
runImpure (Zora m) =
  let
    go step = case step unit of
      More f -> go f
      LiftEffect _ e -> Loop <$> e
      LiftST s -> Loop <$> toEffect s
      Stop a -> pure $ Done a
  in
    tailRecM go \_ ->
      runFn4 m (mkFn2 \f e -> LiftEffect f e) LiftST More Stop

-- | Interpret `Zora` as an `ST`, omitting `Effect`.
runPure :: forall a. Zora a -> ST Global a
runPure (Zora m) =
  let
    go step = case step unit of
      More f -> go f
      LiftEffect f _ -> go f
      LiftST s -> Loop <$> s
      Stop a -> pure $ Done a
  in
    tailRecM go \_ ->
      runFn4 m (mkFn2 \f e -> LiftEffect f e) LiftST More Stop
