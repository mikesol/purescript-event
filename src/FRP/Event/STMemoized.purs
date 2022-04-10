module FRP.Event.STMemoized (STMemoized, run, run', toEvent) where

import Prelude

import Control.Alt (class Alt, (<|>))
import Control.Plus (class Plus, empty)
import Data.Either (Either(..), either)
import Data.Filterable (class Compactable, class Filterable, filterMap, partitionMap)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import FRP.Event (class IsEvent, Event, keepLatest, makeEvent, subscribe)
import FRP.Event.Memoize (isMemoizable, memoizeIfMemoizable)

data STMemoized (r :: Type) (a :: Type)

foreign import mmzMap :: forall r a b. (a -> b) -> STMemoized r a -> STMemoized r b

instance functorSTMemoized :: Functor (STMemoized r) where
  map = mmzMap

foreign import mmzSampleOn :: forall r a b. STMemoized r a -> STMemoized r (a -> b) -> STMemoized r b

foreign import mmzKeepLatest :: forall r a. STMemoized r (STMemoized r a) -> STMemoized r a

foreign import mmzEmpty :: forall r a. STMemoized r a

foreign import mmzBang :: forall r a. a -> STMemoized r a

instance plusSTMemoized :: Plus (STMemoized r) where
  empty = mmzEmpty

foreign import mmzAlt :: forall r a. STMemoized r a -> STMemoized r a -> STMemoized r a

instance altSTMemoized :: Alt (STMemoized r) where
  alt = mmzAlt

foreign import mmzPartitionMap
  :: forall x a l r
   . (a -> Either l r)
  -> STMemoized x a
  -> { left :: STMemoized x l, right :: STMemoized x r }

instance isEventSTMemoized :: IsEvent (STMemoized r) where
  bang = mmzBang
  sampleOn = mmzSampleOn
  keepLatest = mmzKeepLatest
  fold = mmzFold
  fix f = keepLatest
    $ map _.output
    $ fold (\a b -> f (a <|> b.input)) empty { input: empty, output: empty }

instance compactableSTMemoized :: Compactable (STMemoized r) where
  compact = filterMap identity
  separate = partitionMap identity

instance filterableSTMemoized :: Filterable (STMemoized r) where
  partitionMap = mmzPartitionMap
  partition f x =
    let
      { left, right } = partitionMap (\v -> if f v then Left v else Right v) x
    in
      { yes: right, no: left }
  filterMap f = map _.right
    ( partitionMap
        ( \v -> case f v of
            Just x -> Right x
            Nothing -> Left unit
        )
    )
  filter f = filterMap (\v -> if f v then Just v else Nothing)

foreign import mmzFold :: forall r a b. (a -> b -> b) -> STMemoized r a -> b -> STMemoized r b

fold :: forall r a b. (a -> b -> b) -> STMemoized r a -> b -> STMemoized r b
fold = mmzFold

foreign import runMMZ_
  :: forall r a
   . { either :: forall f g m. (f -> m) -> (g -> m) -> Either f g -> m }
  -> r
  -> STMemoized r a
  -> Effect Unit

foreign import addSubscription_
  :: forall r a. (a -> Effect Unit) -> STMemoized r a -> Effect Unit

addSubscription :: forall r a. (a -> Effect Unit) -> STMemoized r a -> Effect Unit
addSubscription = addSubscription_

foreign import removeSubscription_
  :: forall r a. (a -> Effect Unit) -> STMemoized r a -> Effect Unit

removeSubscription :: forall r a. (a -> Effect Unit) -> STMemoized r a -> Effect Unit
removeSubscription = removeSubscription_

foreign import mmzStart_ :: forall r a. Event a -> STMemoized r a

run :: forall a o. Event a -> (forall event. IsEvent event => event a -> o) -> o
run e a = run' e a a

run' :: forall a o. Event a -> (forall r. STMemoized r a -> o) -> (Event a -> o) -> o
run' e' f1 f2 = if isMemoizable e' then go (memoizeIfMemoizable e') else f2 e'
  where
  go e =
    let
      o = mmzStart_ $ makeEvent \_ -> do
        subscribe e \v -> do
          runSTMemoizedInternal v o
    in
      f1 o

foreign import actualizeMMZ_ :: forall r a. STMemoized r a -> Effect Unit

toEvent :: forall r a. STMemoized r a -> Event a
toEvent mmz = makeEvent \k -> do
  actualizeMMZ_ mmz
  addSubscription k mmz
  pure $ removeSubscription k mmz

runSTMemoizedInternal :: forall r a. r -> STMemoized r a -> Effect Unit
runSTMemoizedInternal = runMMZ_ { either }