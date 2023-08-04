module FRP.Event.AnimationFrame
  ( animationFrame
  ) where

import Prelude

import Effect (Effect)
import Effect.Ref as Ref
import FRP.Event (Event, makeEventE)
import Web.HTML (window)
import Web.HTML.Window (requestAnimationFrame)

-- | Create an event which fires every frame (using `requestAnimationFrame`).
animationFrame :: Effect
                             { event :: Event Unit
                             , unsubscribe :: Effect Unit
                             }
animationFrame = makeEventE \k -> do
  w <- window
  cancelled <- Ref.new false
  let
    loop = void do
      w # requestAnimationFrame do
        k unit
        unlessM (Ref.read cancelled) loop
  loop
  pure (Ref.write true cancelled)