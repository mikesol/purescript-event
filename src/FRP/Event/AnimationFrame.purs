module FRP.Event.AnimationFrame
  ( animationFrame'
  , animationFrame
  ) where

import Prelude

import Effect (Effect)
import Effect.Ref as Ref
import FRP.Event (Event, makeEventE)
import Web.HTML (window)
import Web.HTML.Window (requestAnimationFrame)

-- | Create an event which fires every frame (using `requestAnimationFrame`).
animationFrame'
  :: forall a
   . ((a -> Effect Unit) -> Effect Unit)
  -> Effect
       { event :: Event a
       , unsubscribe :: Effect Unit
       }
animationFrame' f = makeEventE \k -> do
  w <- window
  cancelled <- Ref.new false
  let
    loop = void do
      w # requestAnimationFrame do
        f k
        unlessM (Ref.read cancelled) loop
  loop
  pure (Ref.write true cancelled)

animationFrame
  :: Effect
       { event :: Event Unit
       , unsubscribe :: Effect Unit
       }
animationFrame = animationFrame' (_ $ unit)
