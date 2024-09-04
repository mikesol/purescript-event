module FRP.Event.Multiplex
  ( withMultiplexing
  ) where

import Prelude

import Data.Foldable (for_)
import Effect (Effect)

withMultiplexing
  :: forall a b
   . Array ((b -> Effect Unit) -> a -> Effect Unit)
  -> (b -> Effect Unit)
  -> a
  -> Effect Unit
withMultiplexing l op = \a ->
  for_ l \f -> let x = f op in x a
