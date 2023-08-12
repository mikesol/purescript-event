module Test.Main where

import Prelude

import Control.Alt (class Alt, (<|>))
import Control.Monad.ST (ST)
import Control.Monad.ST.Class (liftST)
import Control.Monad.ST.Global (Global)
import Control.Monad.ST.Ref (STRef)
import Control.Monad.ST.Ref as STRef
import Control.Plus (class Plus, empty)
import Data.Array (length, replicate)
import Data.Array as Array
import Data.Filterable (class Filterable, filter)
import Data.Foldable (sequence_)
import Data.JSDate (getTime, now)
import Data.Profunctor (lcmap)
import Data.Traversable (foldr, for_, sequence)
import Data.Tuple (Tuple(..))
import Data.Tuple.Nested ((/\))
import Debug (spy)
import Effect (Effect)
import Effect.Aff (Milliseconds(..), delay, launchAff_)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Ref as Ref
import Effect.Unsafe (unsafePerformEffect)
import FRP.Event (Event, mailbox, makeEvent, makeLemmingEvent, memoized, merge, sampleOnRight, subscribe)
import FRP.Event as Event
import FRP.Event.Class (fold, once, keepLatest, sampleOnRight)
import FRP.Event.Time (debounce)
import FRP.Poll (derivative', fixB, gate, integral', keepLatestHack, poll, sample, sample_, stRefToPoll)
import FRP.Poll as Poll
import Test.Spec (SpecT, describe, it, itOnly)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Console (write)
import Test.Spec.Reporter (consoleReporter)
import Test.Spec.Runner (runSpec)

modify__ :: forall a r. (a -> a) -> STRef r a -> ST r Unit
modify__ a b = void $ STRef.modify a b

fresh :: forall a r. a -> ST r (STRef r a)
fresh = STRef.new

-- Because of limitations in how polymorphism works in PureScript,
-- every test needs to have the same type for the pusher and output.
-- For most tests that's `Int` and `Int`, but for a couple, the input type
-- is `Unit` and the output type is somethign different. So we
-- need new suites for those.
suite1 name { setup, prime, create, toEvent, underTest, kkll } = do
  describe name do
    it "should do simple stuff" $ liftEffect do
      r <- liftST $ STRef.new []
      ep <- liftST setup
      testing <- liftST create
      u1 <- liftST $ subscribe (toEvent (underTest testing) ep) \i ->
        liftST $ void $ STRef.modify (Array.cons i) r
      prime ep
      testing.push 0
      v <- liftST $ STRef.read r
      v `shouldEqual` [ 0 ]
      u2 <- liftST $ subscribe (toEvent (underTest testing) ep) \i ->
        liftST $ void $ STRef.modify (Array.cons (negate i)) r
      v' <- liftST $ STRef.read r
      v' `shouldEqual` [ 0 ]
      prime ep
      testing.push 1
      v'' <- liftST $ STRef.read r
      v'' `shouldEqual` [ -1, 1, 0 ]
      liftST (u1 *> u2)
    it "should do a lot more complex addition" $ liftEffect do
      r <- liftST $ STRef.new []
      ep <- liftST setup
      testing <- liftST create
      let
        testAddition = do
          let
            add1 = map (add 1) (underTest testing)
            add2 = map (add 2) add1
            add3 = map (add 3) add2
            add4 = map (add 4) add3
          add1 <|> add4
      u <- liftST $ subscribe (toEvent testAddition ep) \i ->
        liftST $ void $ STRef.modify (Array.cons i) r
      prime ep
      testing.push 0
      v <- liftST $ STRef.read r
      v `shouldEqual` [ 10, 1 ]
      liftST u
    it "should handle alt" $ liftEffect do
      r <- liftST $ STRef.new []
      ep <- liftST setup
      testing <- liftST create
      let
        testAlt = do
          let
            add1 = (map (add 1) (underTest testing))
            add2 = map (add 2) add1
            add3 = map (add 3) add2
            add4 = map (add 4) add3
            altr = add1 <|> add2 <|> empty <|> add4 <|> empty
          add1 <|> altr
      u <- liftST $ subscribe (toEvent testAlt ep) \i ->
        liftST $ void $ STRef.modify (Array.cons i) r
      prime ep
      testing.push 0
      v <- liftST $ STRef.read r
      v `shouldEqual` [ 10, 3, 1, 1 ]
      liftST u
    it "should handle filter 1" $ liftEffect do
      r <- liftST $ STRef.new []
      ep <- liftST setup
      testing <- liftST create
      let
        filtered = do
          let
            add1 = map (add 1) (underTest testing)
            add2 = map (add 2) add1
            add3 = map (add 3) add2
            add4 = map (add 4) add3
            altr = add1 <|> add2 <|> empty <|> add4 <|> empty
            fm = (filter (_ < 5) altr)
          add1 <|> fm
      u <- liftST $ subscribe (toEvent filtered ep) \i ->
        liftST $ void $ STRef.modify (Array.cons i) r
      prime ep
      testing.push 0
      v <- liftST $ STRef.read r
      v `shouldEqual` [ 3, 1, 1 ]
      liftST u
    it "should handle filter 2" $ liftEffect do
      r <- liftST $ STRef.new []
      ep <- liftST setup
      testing <- liftST create
      let add1 = (map (add 1) (underTest testing))
      let add2 = map (add 2) add1
      let add3 = map (add 3) add2
      let add4 = map (add 4) add3
      let altr = add1 <|> add2 <|> empty <|> add4 <|> empty
      let fm = (filter (_ > 5) altr)
      u <- liftST $ subscribe (toEvent (add1 <|> fm) ep) \i ->
        liftST $ void $ STRef.modify (Array.cons i) r
      prime ep
      testing.push 0
      v <- liftST $ STRef.read r
      v `shouldEqual` [ 10, 1 ]
      liftST u
    it "should sampleOnRight correctly" $ liftEffect do
      r <- liftST $ STRef.new []
      ep <- liftST setup
      testing0 <- liftST create
      testing1 <- liftST create
      let toTest = sampleOnRight (underTest testing0) (add <$> (underTest testing1))
      u <- liftST $ subscribe (toEvent toTest ep) \i ->
        liftST $ void $ STRef.modify (Array.cons i) r
      prime ep
      -- noop
      testing1.push 1
      -- noop
      testing0.push 3
      -- 45
      testing1.push 42
      -- 104
      testing1.push 101
      -- no op
      testing0.push 42
      -- 50
      testing1.push 8
      -- 51
      testing1.push 9
      v <- liftST $ STRef.read r
      v `shouldEqual` [ 51, 50, 104, 45 ]
      liftST u
    itOnly "should keepLatest" $ liftEffect do
      r <- liftST $ STRef.new []
      ep <- liftST setup
      testing0 <- liftST create
      testing1 <- liftST create
      let toTest = kkll (underTest testing0 $> underTest testing1)
      u <- liftST $ subscribe (toEvent toTest ep) \i ->
        liftST $ void $ STRef.modify (Array.cons i) r
      prime ep
      testing0.push 42
      testing1.push 3
      testing1.push 4
      testing0.push 42
      testing1.push 5
      testing1.push 6
      v <- liftST $ STRef.read r
      v `shouldEqual` [ 6, 5, 4, 3 ]
      liftST u
  it "should keep itself when keepLatest is used" $ liftEffect do
    r <- liftST $ STRef.new []
    ep <- liftST setup
    testing <- liftST $ create
    let tested = underTest testing
    u1 <- liftST $ subscribe (toEvent (kkll (tested $> tested)) ep) \i -> do
      --let _ = spy "incoming" true
      liftST $ void $ STRef.modify (Array.cons i) r
    --let _ = spy "pre prime" {}
    prime ep
    --let _ = spy "pre push 0" {}
    testing.push 0
    --let _ = spy "pre push 1" {}
    testing.push 1
    --let _ = spy "pre push 42" {}
    testing.push 42
    --let _ = spy "post push 42" {}
    v <- liftST $ STRef.read r
    v `shouldEqual` [ 42, 1, 0 ]
    liftST u1

suite2 name { setup, prime, create, toEvent, underTest } = do
  describe name do
    it "should handle fold 1" $ liftEffect do
      r <- liftST $ STRef.new []
      ep <- liftST setup
      testing <- liftST create
      let
        event' = do
          let foldy = (fold (\b _ -> b + 1) 0 (underTest testing))
          let add2 = map (add 2) foldy
          let add3 = map (add 3) add2
          let add4 = map (add 4) add3
          let altr = foldy <|> add2 <|> empty <|> add4 <|> empty
          let fm = (filter (_ > 5) altr)
          foldy <|> fm
      u <- liftST $ subscribe (toEvent event' ep) \i ->
        liftST $ void $ STRef.modify (Array.cons i) r
      prime ep
      testing.push unit
      liftST (STRef.read r) >>= shouldEqual [ 10, 1 ]
      liftST $ void $ STRef.write [] r
      testing.push unit
      liftST (STRef.read r) >>= shouldEqual [ 11, 2 ]
      liftST $ void $ STRef.write [] r
      testing.push unit
      liftST (STRef.read r) >>= shouldEqual [ 12, 3 ]
      liftST u

suite3 name { setup, prime, create, toEvent, underTest } = do
  describe name do
    it "should handle fold 2" do
      liftEffect do
        r <- liftST $ STRef.new []
        ep <- liftST setup
        testing <- liftST create
        let
          event' = do
            let add1 = map (add 1) (underTest testing)
            let add2 = map (add 2) add1
            let add3 = map (add 3) add2
            let foldy = fold (\b a -> a + b) 0 add3
            let add4 = map (add 4) add3
            let altr = foldy <|> add2 <|> empty <|> add4 <|> empty
            sampleOnRight add2 (map (\a b -> b /\ a) (filter (_ > 5) altr))
        u <- liftST $ subscribe (toEvent event' ep) \i ->
          liftST $ void $ STRef.modify (Array.cons i) r
        prime ep
        testing.push 0
        liftST (STRef.read r) >>= shouldEqual [ Tuple 3 10, Tuple 3 6 ]
        liftST $ void $ STRef.write [] r
        testing.push 0
        liftST (STRef.read r) >>= shouldEqual [ Tuple 3 10, Tuple 3 12 ]
        liftST $ void $ STRef.write [] r
        testing.push 0
        liftST (STRef.read r) >>= shouldEqual [ Tuple 3 10, Tuple 3 18 ]
        liftST u

suite4 name { setup, prime, create, toEvent, underTest } = do
  describe name do
    it "should ignore left pushes on initial event but respond to both right pushes" $ liftEffect do
      r <- liftST $ STRef.new []
      ep <- liftST setup
      testing <- liftST create
      let
        e e' = Tuple <$> (e' $> 1 <|> e' $> 2) <*> (e' $> 3 <|> e' $> 4)
      u <- liftST $ subscribe (toEvent (e (underTest testing)) ep) \i ->
        liftST $ void $ STRef.modify (flip Array.snoc i) r
      prime ep
      testing.push unit
      liftST (STRef.read r) >>= \y -> y `shouldEqual` [Tuple 2 3, Tuple 2 4]
      liftST u

suite5 name { setup, prime, create, toEvent, underTest } = do
  describe name do
    it "respects both sides of application" $ liftEffect do
      testing <- liftST create
      ep <- liftST setup
      rf0 <- liftST $ STRef.new ""
      rf1 <- liftST $ STRef.new ""
      let tested = underTest testing
      u1 <- liftST $ Event.subscribe (toEvent ((append <$> (once tested $> "a")) <*> tested) ep) (liftST <<< void <<< flip STRef.write rf0)
      u2 <- liftST $ Event.subscribe (toEvent ((append <$> tested) <*> (once tested $> "b")) ep) (liftST <<< void <<< flip STRef.write rf1)
      prime ep
      testing.push "c"
      rf0' <- liftST $ STRef.read rf0
      rf1' <- liftST $ STRef.read rf1
      rf0' `shouldEqual` "ac"
      rf1' `shouldEqual` "cb"
      liftST $ u1 *> u2

main :: Effect Unit
main = do
  launchAff_
    $ runSpec [ consoleReporter ] do
        suite1 "Poll"
          { setup: Event.create
          , prime: \ep -> ep.push unit
          , create: Poll.create
          , toEvent: \b ep -> sample_ b ep.event
          , underTest: \testing -> testing.poll
          , kkll: keepLatestHack
          }
        -- suite1 "Event"
        --   { setup: pure unit
        --   , prime: pure
        --   , create: Event.create
        --   , toEvent: \e _ -> e
        --   , underTest: \testing -> testing.event
        --   , kkll: keepLatest
        --   }
        suite2 "Poll"
          { setup: Event.create
          , prime: \ep -> ep.push unit
          , create: Poll.create
          , toEvent: \b ep -> sample_ b ep.event
          , underTest: \testing -> testing.poll
          }
        suite2 "Event"
          { setup: pure unit
          , prime: pure
          , create: Event.create
          , toEvent: \e _ -> e
          , underTest: \testing -> testing.event
          }

        suite3 "Poll"
          { setup: Event.create
          , prime: \ep -> ep.push unit
          , create: Poll.create
          , toEvent: \b ep -> sample_ b ep.event
          , underTest: \testing -> testing.poll
          }
        suite3 "Event"
          { setup: pure unit
          , prime: pure
          , create: Event.create
          , toEvent: \e _ -> e
          , underTest: \testing -> testing.event
          }
        suite4 "Poll"
          { setup: Event.create
          , desc: "should distribute apply to all behaviors"
          , prime: \ep -> ep.push unit
          , create: Poll.create
          , toEvent: \b ep -> sample_ b ep.event
          , underTest: \testing -> testing.poll
          }
        suite4 "Event"
          { setup: pure unit
          , desc: "should ignore left pushes on initial event but respond to both right pushes"
          , prime: pure
          , create: Event.create
          , toEvent: \e _ -> e
          , underTest: \testing -> testing.event
          }
        suite5 "Event"
          { setup: pure unit
          , prime: pure
          , create: Event.create
          , toEvent: \e _ -> e
          , underTest: \testing -> testing.event
          }
        suite5 "Poll"
          { setup: Event.create
          , prime: \ep -> ep.push unit
          , create: Poll.create
          , toEvent: \b ep -> sample_ b ep.event
          , underTest: \testing -> testing.poll
          }

        describe "Unique to Poll" do
          it "should switch" $ liftEffect do
                r <- liftST $ STRef.new []
                switchDriver <- liftST $ Event.create
                poller <- liftST $ Event.create
                u <- liftST $ subscribe (sample_ (Poll.switcher empty switchDriver.event) poller.event) \i ->
                  liftST $ void $ STRef.modify (Array.cons i) r
                poller.push unit
                switchDriver.push (pure 42)
                poller.push unit
                poller.push unit
                switchDriver.push (pure 43)
                poller.push unit
                switchDriver.push (pure 44)
                switchDriver.push (pure 45)
                poller.push unit
                poller.push unit
                v <- liftST $ STRef.read r
                v `shouldEqual` [ 45, 45, 43, 42, 42 ]
                liftST u
        describe "Unique to Event" do
          -- this test shows how a poll based framework could be used
          -- to emit html, where the webpage is a poll and it is
          -- rendered based on an initial event
          it "should fire in order for polls" $ liftEffect do
            r <- liftST $ STRef.new []
            ep <- liftST $ Event.create
            let
              bhv c = poll \e0 -> makeLemmingEvent \s0 k0 -> s0 e0 \f0 -> do
                -- first element
                k0 (f0 "div")
                void $ flip s0 k0 $ flip sample e0 $ poll \e1 ->
                  merge
                    [ flip sample e1
                        $ poll \e2 -> makeLemmingEvent \s2 k2 -> s2 e2 \f2 -> k2 (f2 "span")
                    , flip sample e1 $ c
                    , flip sample e1
                        $ poll \e2 -> makeLemmingEvent \s2 k2 -> s2 e2 \f2 -> k2 (f2 "b")
                    ]
            u <- liftST $ subscribe (sample (bhv (bhv (bhv ((bhv $ poll \e2 -> makeLemmingEvent \s2 k2 -> s2 e2 \f2 -> k2 (f2 "h3")))))) ep.event) \i ->
              liftST $ void $ STRef.modify (flip Array.snoc i) r
            ep.push identity
            v <- liftST $ STRef.read r
            v `shouldEqual`
              [
                -- first level
                "div"
              , "span"
              ,
                -- second level
                "div"
              , "span"
              ,
                -- third level
                "div"
              , "span"
              ,
                -- fourth level
                "div"
              , "span"
              , "h3"
              , "b"
              ,
                -- third level
                "b"
              ,
                -- second level
                "b"
              ,
                -- first level
                "b"
              ]
            liftST u
          it "should fire in order for polls 2" $ liftEffect do
            r <- liftST $ STRef.new []
            ep <- liftST $ Event.create
            let
              bhv c = poll \e0 -> makeLemmingEvent \s0 k0 -> s0 e0 \f0 -> do
                -- first element
                k0 (f0 "div")
                void $ flip s0 k0 $ flip sample e0 $ poll \e1 ->
                  merge
                    [ flip sample e1
                        $ poll \e2 -> makeLemmingEvent \s2 k2 -> s2 e2 \f2 -> k2 (f2 "span")
                    , flip sample e1 $ c
                    , flip sample e1
                        $ poll \e2 -> makeLemmingEvent \s2 k2 -> s2 e2 \f2 -> k2 (f2 "b")
                    , flip sample e1 $ c
                    ]
            u <- liftST $ subscribe (sample (bhv (bhv $ poll \e2 -> makeLemmingEvent \s2 k2 -> s2 e2 \f2 -> k2 (f2 "h3"))) ep.event) \i ->
              liftST $ void $ STRef.modify (flip Array.snoc i) r
            ep.push identity
            v <- liftST $ STRef.read r
            v `shouldEqual`
              [
                -- first level
                "div"
              , "span"
              ,
                -- second level
                "div"
              , "span"
              , "h3"
              , "b"
              , "h3"
              -- first level
              , "b"
              , -- second level
                "div"
              , "span"
              , "h3"
              , "b"
              , "h3"
              ]
            liftST u
          it "should respond correctly to internal pushes" $ liftEffect do
            r <- liftST $ STRef.new []
            ep <- liftST $ Event.create
            let
              evt = makeEvent \k -> Event.subscribe ep.event \i -> do
                k i
                when i (ep.push (not i))
            u <- liftST $ subscribe evt \i ->
              liftST $ void $ STRef.modify (flip Array.snoc i) r
            ep.push true
            v <- liftST $ STRef.read r
            v `shouldEqual`
              [ true
              , false
              ]
            liftST u
          describe "Performance" do
            it "handles 10 subscriptions with a simple event and 1000 pushes" $ liftEffect do
              starts <- getTime <$> now
              r <- liftST $ STRef.new []
              { push, event } <- liftST $ Event.create
              us <- liftST $ sequence $ replicate 10 $ subscribe (map (add 1) $ map (add 1) event) \i ->
                liftST $ void $ STRef.modify (Array.cons i) r
              for_ (replicate 1000 3) push
              liftST $ sequence_ us
              ends <- getTime <$> now
              write ("Duration: " <> show (ends - starts) <> "\n")
            it "handles 1000 subscriptions with a simple event and 10 pushes" $ liftEffect do
              starts <- getTime <$> now
              r <- liftST $ STRef.new []
              { push, event } <- liftST $ Event.create
              us <- liftST $ sequence $ replicate 1000 $ subscribe (map (add 1) $ map (add 1) event) \i ->
                liftST $ void $ STRef.modify (Array.cons i) r
              for_ (replicate 10 3) push
              liftST $ sequence_ us
              ends <- getTime <$> now
              write ("Duration: " <> show (ends - starts) <> "\n")
            it "handles 1 subscription with a 10-nested event + 100 alts and 100 pushes" $ liftEffect do
              starts <- getTime <$> now
              r <- liftST $ STRef.new []
              { push, event } <- liftST $ Event.create
              let e = merge $ replicate 100 $ foldr ($) event (replicate 10 (map (add 1)))
              u <- liftST $ subscribe e \i -> liftST $ void $ STRef.modify (Array.cons i) r
              for_ (replicate 100 3) push
              liftST u
              ends <- getTime <$> now
              write ("Duration: " <> show (ends - starts) <> "\n")
            it "handles 1 subscription with a 10-nested event + array of 100 and 100 pushes" $ liftEffect do
              starts <- getTime <$> now
              r <- liftST $ STRef.new []
              { push, event } <- liftST $ Event.create
              let event' = map (replicate 100) $ foldr ($) event (replicate 10 (map (add 1)))
              u <- liftST $ subscribe event' \i ->
                liftST $ void $ STRef.modify (Array.cons i) r
              for_ (replicate 100 3) push
              liftST u
              ends <- getTime <$> now
              write ("Duration: " <> show (ends - starts) <> "\n")
          describe "Memoization" do
            it "should not memoize" $ liftEffect do
              { push, event } <- liftST Event.create
              count <- Ref.new 0
              let
                fn v =
                  unsafePerformEffect do
                    Ref.modify_ (add 1) count
                    pure $ v
              let mapped = identity (map fn event)
              u1 <- liftST $ Event.subscribe mapped (pure (pure unit))
              u2 <- liftST $ Event.subscribe mapped (pure (pure unit))
              push 0
              Ref.read count >>= shouldEqual 2
              liftST u1
              liftST u2
            it "should memoize" $ liftEffect do
              { push, event } <- liftST $ Event.create
              count <- liftST $ STRef.new 0
              let
                fn v =
                  unsafePerformEffect do
                    void $ liftST $ STRef.modify (add 1) count
                    pure $ v
                mapped = keepLatest $
                  memoized (identity (map fn event)) \e -> Event.makeEvent \k -> do
                    u1 <- Event.subscribe e (\_ -> pure unit)
                    u2 <- Event.subscribe e k
                    pure (u1 *> u2)
              u <- liftST $ Event.subscribe mapped (\_ -> pure unit)
              push 0
              (liftST $ STRef.read count) >>= shouldEqual 1
              liftST $ u
            it "should not memoize when applied internally" $ liftEffect do
              { push, event } <- liftST $ Event.create
              count <- liftST $ STRef.new 0
              let
                fn v =
                  unsafePerformEffect do
                    void $ liftST $ STRef.modify (add 1) count
                    pure $ v
                mapped = keepLatest
                  $ memoized event
                  $ (lcmap (identity <<< map fn)) \e ->
                      Event.makeEvent \k -> do
                        u1 <- liftST $ Event.subscribe e (\_ -> pure unit)
                        u2 <- liftST $ Event.subscribe e k
                        pure (u1 *> u2)
              u <- liftST $ Event.subscribe mapped (\_ -> pure unit)
              push 0
              (liftST $ STRef.read count) >>= shouldEqual 2
              liftST $ u
          describe "Apply" do
            it "respects both sides of application" $ liftEffect do
              { event, push } <- liftST $ Event.create
              rf0 <- liftST $ STRef.new ""
              rf1 <- liftST $ STRef.new ""
              void $ liftST $ Event.subscribe ((append <$> (once event $> "a")) <*> event) (liftST <<< void <<< flip STRef.write rf0)
              void $ liftST $ Event.subscribe ((append <$> event) <*> (once event $> "b")) (liftST <<< void <<< flip STRef.write rf1)
              push "c"
              rf0' <- liftST $ STRef.read rf0
              rf1' <- liftST $ STRef.read rf1
              rf0' `shouldEqual` "ac"
              rf1' `shouldEqual` "cb"
            it "always applies updates from left to right, emitting at each update" $ liftEffect do
              r <- liftST $ STRef.new []
              { push, event } <- liftST $ Event.create
              u <- liftST $ Event.subscribe (let x = event in (map add x) <*> x) \i ->
                liftST $ void $ STRef.modify (flip Array.snoc i) r
              push 1
              push 2
              o <- liftST $ STRef.read r
              o `shouldEqual` [ 2, 3, 4 ]
              liftST $ u
            it "always applies multiple updates from left to right, emitting at each update" $ liftEffect do
              r <- liftST $ STRef.new []
              { push, event } <- liftST $ Event.create
              let addSixNums x y z a b c = x + y + z + a + b + c
              u <- liftST $ Event.subscribe (let x = event in addSixNums <$> x <*> x <*> x <*> x <*> x <*> x) \i ->
                liftST $ void $ STRef.modify (flip Array.snoc i) r
              push 1
              push 2
              o <- liftST $ STRef.read r
              o `shouldEqual` [ 6, 7, 8, 9, 10, 11, 12 ]
              liftST $ u
          describe "Mailboxed" do
            it "should work" $ liftEffect do
              r <- liftST $ STRef.new []
              e <- liftST $ mailbox
              u <- liftST $ Event.subscribe (e.event 3 <|> e.event 4) \i ->
                liftST $ void $ STRef.modify (Array.cons i) r
              do
                e.push { address: 42, payload: true }
                e.push { address: 43, payload: true }
                e.push { address: 44, payload: true }
                e.push { address: 3, payload: true } --
                e.push { address: 42, payload: false }
                e.push { address: 43, payload: true }
                e.push { address: 43, payload: false }
                e.push { address: 4, payload: false } --
                e.push { address: 42, payload: false }
                e.push { address: 43, payload: true }
                e.push { address: 3, payload: false } --
                e.push { address: 101, payload: true }
              o <- liftST $ STRef.read r
              o `shouldEqual` [ false, false, true ]
              liftST $ u
          describe "Mailbox" do
            it "should work" $ liftEffect do
              r <- liftST $ STRef.new []
              e <- liftST $ Event.mailbox
              u <- liftST $ Event.subscribe (e.event 3 <|> e.event 4) \i ->
                liftST $ void $ STRef.modify (Array.cons i) r
              do
                e.push { address: 42, payload: true }
                e.push { address: 43, payload: true }
                e.push { address: 44, payload: true }
                e.push { address: 3, payload: true } --
                e.push { address: 42, payload: false }
                e.push { address: 43, payload: true }
                e.push { address: 43, payload: false }
                e.push { address: 4, payload: false } --
                e.push { address: 42, payload: false }
                e.push { address: 43, payload: true }
                e.push { address: 3, payload: false } --
                e.push { address: 101, payload: true }
              o <- liftST $ STRef.read r
              o `shouldEqual` [ false, false, true ]
              liftST $ u
          describe "Gate" do
            it "should work" $ liftEffect do
              eio <- liftST $ Event.create
              r <- liftST $ STRef.new false
              n <- liftST $ STRef.new 0
              let b = stRefToPoll r
              _ <- liftST $ Event.subscribe (gate b eio.event) \_ ->
                liftST $ void $ STRef.modify (add 1) n
              do
                eio.push unit
                eio.push unit
              liftST $ void $ STRef.write true r
              do
                eio.push unit
                eio.push unit
                eio.push unit
              liftST $ void $ STRef.write false r
              do
                eio.push unit
                eio.push unit
              res <- liftST $ STRef.read n
              shouldEqual res 3

        describe "Miscellaneous" do
          describe "Fix" do
            it "should work" do
              { event, push } <- liftST $ Event.create
              rf <- liftEffect $ Ref.new []
              unsub <- liftST $ Event.subscribe (debounce (Milliseconds 1000.0) event) (\i -> Ref.modify_ (Array.cons i) rf)
              liftEffect do
                push 1
                push 2
                push 3
                push 4
              delay (Milliseconds 1500.0)
              liftEffect do
                push 5
                push 6
                o <- Ref.read rf
                o `shouldEqual` [ 5, 1 ]
                liftST $ unsub

          describe "derivative" do
            it "should give some sane approximation" do
              { event, push } <- liftST $ Event.create
              rf <- liftEffect $ Ref.new []
              unsub <- liftST $ Event.subscribe (sample_ (derivative' (fixB 1.0 (map (add 1.0))) (fixB 1.0 (map (mul 3.0)))) event) (\i -> Ref.modify_ (Array.cons i) rf)
              liftEffect do
                push unit
                push unit
                push unit
                push unit
                o <- Ref.read rf
                o `shouldEqual` [ 54.0, 18.0, 6.0, 0.0 ]
                liftST $ unsub

          describe "integral" do
            it "should give some sane approximation" do
              { event, push } <- liftST $ Event.create
              rf <- liftEffect $ Ref.new []
              unsub <- liftST $ Event.subscribe (sample_ (integral' 42.0 (fixB 1.0 (map (add 1.0))) (fixB 1.0 (map (mul 3.0)))) event) (\i -> Ref.modify_ (Array.cons i) rf)
              liftEffect do
                push unit
                push unit
                push unit
                push unit
                o <- Ref.read rf
                o `shouldEqual` [ 120.0, 66.0, 48.0, 42.0 ]
                liftST $ unsub
          describe "FixB" do
            it "should work" do
              { event, push } <- liftST $ Event.create
              rf <- liftEffect $ Ref.new []
              unsub <- liftST $ Event.subscribe (sample_ (fixB 0 (map (add 1))) event) (\i -> Ref.modify_ (Array.cons i) rf)
              liftEffect do
                push unit
                push unit
                push unit
                push unit
                o <- Ref.read rf
                o `shouldEqual` [ 4, 3, 2, 1 ]
                liftST $ unsub

          describe "Debounce" do
            it "debounces" do
              let
                f emitSecond = do
                  { event, push } <- liftST $ Event.create
                  rf <- liftEffect $ Ref.new []
                  unsub <- liftST $ Event.subscribe (debounce (Milliseconds 500.0) event) (\i -> Ref.modify_ (Array.cons i) rf)
                  liftEffect $ push unit
                  when emitSecond do
                    liftEffect $ push unit
                  delay $ Milliseconds 250.0
                  liftEffect $ push unit
                  delay $ Milliseconds 300.0
                  liftEffect $ push unit
                  liftEffect $ push unit
                  o <- liftEffect $ Ref.read rf
                  length o `shouldEqual` 2
                  liftST $ unsub
              f true
              f false
