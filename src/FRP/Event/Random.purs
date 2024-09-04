module FRP.Event.Random where

import Prelude

import Effect (Effect)
import Effect.Random (random)

withRandom :: forall a. ({ value :: a, random :: Number } -> Effect Unit) -> a -> Effect Unit
withRandom = go
  where
  go f value = do
    r <- random
    f { random: r, value }
