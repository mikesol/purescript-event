module Test.Main where

import Prelude

import Control.Alt ((<|>))
import Control.Plus (empty)
import Data.Array (cons)
import Data.Filterable (filter)
import Data.Tuple (Tuple(..))
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import Effect.Ref as Ref
import Effect.Unsafe (unsafePerformEffect)
import FRP.Event (sampleOn)
import FRP.Event as Event
import FRP.Event.Class (class IsEvent, fold)
import FRP.Event.Legacy as Legacy
import FRP.Event.Memoized (memoize)
import FRP.Event.Memoized as Memoized
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Reporter (consoleReporter)
import Test.Spec.Runner (runSpec)

main :: Effect Unit
main = do
  launchAff_
    $ runSpec [ consoleReporter ] do
        let
          suite :: forall event. IsEvent event => String -> (forall a. a -> event a) -> (forall a. Effect { push :: a -> Effect Unit, event :: event a }) -> (forall a. event a -> (a -> Effect Unit) -> Effect (Effect Unit)) -> Spec Unit
          suite name bang create subscribe =
            describe ("Testing " <> name) do
              it "should do simple stuff" do
                liftEffect do
                  rf <- Ref.new []
                  unsub <- subscribe ((bang 0)) \i -> Ref.modify_ (cons i) rf
                  o <- liftEffect $ Ref.read rf
                  o `shouldEqual` [ 0 ]
                  unsub
              it "should do complex stuff" do
                liftEffect do
                  rf <- Ref.new []
                  { push, event } <- create
                  let event' = event
                  unsub1 <- subscribe event' \i -> Ref.modify_ (cons i) rf
                  push 0
                  o <- liftEffect $ Ref.read rf
                  o `shouldEqual` [ 0 ]
                  unsub2 <- subscribe event' \i -> Ref.modify_ (cons (negate i)) rf
                  o' <- liftEffect $ Ref.read rf
                  o' `shouldEqual` [ 0 ]
                  push 1
                  o'' <- liftEffect $ Ref.read rf
                  o'' `shouldEqual` [ -1, 1, 0 ]
                  unsub1 *> unsub2
              it "should do a lot more complex addition" do
                liftEffect do
                  rf <- Ref.new []
                  let add1 = (map (add 1) (bang 0))
                  let add2 = map (add 2) add1
                  let add3 = map (add 3) add2
                  let add4 = (map (add 4) add3)
                  unsub <- subscribe (add1 <|> add4) \i -> Ref.modify_ (cons i) rf
                  o <- liftEffect $ Ref.read rf
                  o `shouldEqual` [ 10, 1 ]
                  unsub
              it "should handle alt" do
                liftEffect do
                  rf <- Ref.new []
                  let add1 = (map (add 1) (bang 0))
                  let add2 = map (add 2) add1
                  let add3 = map (add 3) add2
                  let add4 = map (add 4) add3
                  let altr = (add1 <|> add2 <|> empty <|> add4 <|> empty)
                  unsub <- subscribe (add1 <|> altr) \i -> Ref.modify_ (cons i) rf
                  o <- liftEffect $ Ref.read rf
                  o `shouldEqual` [ 10, 3, 1, 1 ]
                  unsub
              it "should handle filter 1" do
                liftEffect do
                  rf <- Ref.new []
                  let add1 = map (add 1) (bang 0)
                  let add2 = map (add 2) add1
                  let add3 = map (add 3) add2
                  let add4 = map (add 4) add3
                  let altr = add1 <|> add2 <|> empty <|> add4 <|> empty
                  let fm = (filter (_ < 5) altr)
                  unsub <- subscribe (add1 <|> fm) (\i -> Ref.modify_ (cons i) rf)
                  o <- liftEffect $ Ref.read rf
                  o `shouldEqual` [ 3, 1, 1 ]
                  unsub
              it "should handle filter 2" do
                liftEffect do
                  rf <- liftEffect $ Ref.new []
                  let add1 = (map (add 1) (bang 0))
                  let add2 = map (add 2) add1
                  let add3 = map (add 3) add2
                  let add4 = map (add 4) add3
                  let altr = add1 <|> add2 <|> empty <|> add4 <|> empty
                  let fm = (filter (_ > 5) altr)
                  unsub <- subscribe (add1 <|> fm) (\i -> Ref.modify_ (cons i) rf)
                  o <- liftEffect $ Ref.read rf
                  o `shouldEqual` [ 10, 1 ]
                  unsub
              it "should handle fold 0" do
                liftEffect do
                  rf <- Ref.new []
                  { push, event } <- create
                  let foldy = (fold (\_ b -> b + 1) event 0)
                  let add2 = map (add 2) foldy
                  let add3 = map (add 3) add2
                  let add4 = map (add 4) add3
                  let altr = foldy <|> add2 <|> empty <|> add4 <|> empty
                  let fm = (filter (_ > 5) altr)
                  unsub <- subscribe (foldy <|> fm) (\i -> Ref.modify_ (cons i) rf)
                  push unit
                  Ref.read rf >>= shouldEqual [ 10, 1 ]
                  Ref.write [] rf
                  push unit
                  Ref.read rf >>= shouldEqual [ 11, 2 ]
                  Ref.write [] rf
                  push unit
                  Ref.read rf >>= shouldEqual [ 12, 3 ]
                  unsub
              it "should handle fold 1" do
                rf <- liftEffect $ Ref.new []
                liftEffect do
                  { push, event } <- create
                  let add1 = map (add 1) event
                  let add2 = map (add 2) add1
                  let add3 = map (add 3) add2
                  let foldy = fold (\a b -> a + b) add3 0
                  let add4 = map (add 4) add3
                  let altr = foldy <|> add2 <|> empty <|> add4 <|> empty
                  let fm = sampleOn add2 (map (\a b -> b /\ a) (filter (_ > 5) altr))
                  unsub <- subscribe fm (\i -> Ref.modify_ (cons i) rf)
                  push 0
                  Ref.read rf >>= shouldEqual [ Tuple 3 10, Tuple 3 6 ]
                  Ref.write [] rf
                  push 0
                  Ref.read rf >>= shouldEqual [ Tuple 3 10, Tuple 3 12 ]
                  Ref.write [] rf
                  push 0
                  Ref.read rf >>= shouldEqual [ Tuple 3 10, Tuple 3 18 ]
                  unsub
        suite "Event" Event.bang Event.create Event.subscribe
        suite "Legacy" pure Legacy.create Legacy.subscribe
        suite "Memoized" Memoized.bang Memoized.create Memoized.subscribe
        describe "Testing memoization" do
          it "should not memoize" do
            liftEffect do
              { push, event } <- Event.create
              count <- Ref.new 0
              let
                fn v =
                  unsafePerformEffect do
                    Ref.modify_ (add 1) count
                    pure $ v
              let mapped = identity (map fn event)
              unsub1 <- Event.subscribe mapped (pure (pure unit))
              unsub2 <- Event.subscribe mapped (pure (pure unit))
              push 0
              Ref.read count >>= shouldEqual 2
              unsub1
              unsub2

          it "should memoize" do
            liftEffect do
              { push, event } <- Event.create
              count <- Ref.new 0
              let
                fn v =
                  unsafePerformEffect do
                    Ref.modify_ (add 1) count
                    pure $ v
              mapped <- memoize (map fn event)
              unsub1 <- Event.subscribe mapped (pure (pure unit))
              unsub2 <- Event.subscribe mapped (pure (pure unit))
              push 0
              Ref.read count >>= shouldEqual 1
              unsub1
              unsub2
          it "should memoize when using memoize" do
            liftEffect do
              { push, event } <- Memoized.create
              count <- Ref.new 0
              let
                fn v =
                  unsafePerformEffect do
                    Ref.modify_ (add 1) count
                    pure $ v
              let mapped = identity (map fn event)
              unsub1 <- Memoized.subscribe mapped (pure (pure unit))
              unsub2 <- Memoized.subscribe mapped (pure (pure unit))
              push 0
              Ref.read count >>= shouldEqual 1
              unsub1
              unsub2
