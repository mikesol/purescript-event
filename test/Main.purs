module Test.Main where

import Prelude

import Control.Alt ((<|>))
import Control.Monad.ST (ST)
import Control.Monad.ST.Class (liftST)
import Control.Monad.ST.Ref (STRef)
import Control.Monad.ST.Ref as STRef
import Control.Plus (empty)
import Data.Array (length, replicate)
import Data.Array as Array
import Data.Filterable (filter)
import Data.Foldable (sequence_)
import Data.JSDate (getTime, now)
import Data.Profunctor (lcmap)
import Data.Traversable (foldr, for_, sequence)
import Data.Tuple (Tuple(..))
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Effect.Aff (Milliseconds(..), delay, launchAff_)
import Effect.Class (liftEffect)
import Effect.Ref as Ref
import Effect.Unsafe (unsafePerformEffect)
import FRP.Behavior (derivative', fixB, gate, integral', sample_, stRefToBehavior)
import FRP.Event (Event, keepLatest, mailbox, memoize, merge, once, sampleOnRight, subscribe)
import FRP.Event as Event
import FRP.Event.Class (fold)
import FRP.Event.Time (debounce)
import Test.Spec (describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Console (write)
import Test.Spec.Reporter (consoleReporter)
import Test.Spec.Runner (runSpec)

modify__ :: forall a r. (a -> a) -> STRef r a -> ST r Unit
modify__ a b = void $ STRef.modify a b

fresh :: forall a r. a -> ST r (STRef r a)
fresh = STRef.new

main :: Effect Unit
main = do
  launchAff_
    $ runSpec [ consoleReporter ] do
        describe "Event" do
          it "should do simple stuff" $ liftEffect do
            r <- liftST $ STRef.new []
            { push, event } <- liftST $ Event.create
            u1 <- liftST $ subscribe (event) \i ->
              liftST $ void $ STRef.modify (Array.cons i) r
            push 0
            v <- liftST $ STRef.read r
            v `shouldEqual` [ 0 ]
            u2 <- liftST $ subscribe (event) \i ->
              liftST $ void $ STRef.modify (Array.cons (negate i)) r
            v' <- liftST $ STRef.read r
            v' `shouldEqual` [ 0 ]
            push 1
            v'' <- liftST $ STRef.read r
            v'' `shouldEqual` [ -1, 1, 0 ]
            liftST (u1 *> u2)
          it "should do a lot more complex addition" $ liftEffect do
            r <- liftST $ STRef.new []
            ep <- liftST $ Event.create
            let
              event = do
                let
                  add1 = map (add 1) ep.event
                  add2 = map (add 2) add1
                  add3 = map (add 3) add2
                  add4 = map (add 4) add3
                add1 <|> add4
            u <- liftST $ subscribe event \i ->
              liftST $ void $ STRef.modify (Array.cons i) r
            ep.push 0
            v <- liftST $ STRef.read r
            v `shouldEqual` [ 10, 1 ]
            liftST u
          it "should handle alt" $ liftEffect do
            r <- liftST $ STRef.new []
            ep <- liftST $ Event.create
            let
              event = do
                let
                  add1 = (map (add 1) ep.event)
                  add2 = map (add 2) add1
                  add3 = map (add 3) add2
                  add4 = map (add 4) add3
                  altr = add1 <|> add2 <|> empty <|> add4 <|> empty
                add1 <|> altr
            u <- liftST $ subscribe event \i ->
              liftST $ void $ STRef.modify (Array.cons i) r
            ep.push 0
            v <- liftST $ STRef.read r
            v `shouldEqual` [ 10, 3, 1, 1 ]
            liftST u
          it "should handle filter 1" $ liftEffect do
            r <- liftST $ STRef.new []
            ep <- liftST $ Event.create
            let
              event = do
                let
                  add1 = map (add 1) ep.event
                  add2 = map (add 2) add1
                  add3 = map (add 3) add2
                  add4 = map (add 4) add3
                  altr = add1 <|> add2 <|> empty <|> add4 <|> empty
                  fm = (filter (_ < 5) altr)
                add1 <|> fm
            u <- liftST $ subscribe event \i ->
              liftST $ void $ STRef.modify (Array.cons i) r
            ep.push 0
            v <- liftST $ STRef.read r
            v `shouldEqual` [ 3, 1, 1 ]
            liftST u
          it "should handle filter 2" $ liftEffect do
            r <- liftST $ STRef.new []
            ep <- liftST $ Event.create
            let add1 = (map (add 1) ep.event)
            let add2 = map (add 2) add1
            let add3 = map (add 3) add2
            let add4 = map (add 4) add3
            let altr = add1 <|> add2 <|> empty <|> add4 <|> empty
            let fm = (filter (_ > 5) altr)
            u <- liftST $ subscribe (add1 <|> fm) \i ->
              liftST $ void $ STRef.modify (Array.cons i) r
            ep.push 0
            v <- liftST $ STRef.read r
            v `shouldEqual` [ 10, 1 ]
            liftST u
          it "should handle fold 0" $ liftEffect do
            r <- liftST $ STRef.new []
            { push, event } <- liftST $ Event.create
            let
              event' = do
                let foldy = (fold (\b _ -> b + 1) 0 event)
                let add2 = map (add 2) foldy
                let add3 = map (add 3) add2
                let add4 = map (add 4) add3
                let altr = foldy <|> add2 <|> empty <|> add4 <|> empty
                let fm = (filter (_ > 5) altr)
                foldy <|> fm
            u <- liftST $ subscribe event' \i ->
              liftST $ void $ STRef.modify (Array.cons i) r
            push unit
            liftST (STRef.read r) >>= shouldEqual [ 10, 1 ]
            liftST $ void $ STRef.write [] r
            push unit
            liftST (STRef.read r) >>= shouldEqual [ 11, 2 ]
            liftST $ void $ STRef.write [] r
            push unit
            liftST (STRef.read r) >>= shouldEqual [ 12, 3 ]
            liftST u
          it "should handle fold 1" do
            liftEffect do
              r <- liftST $ STRef.new []
              { push, event } <- liftST $ Event.create
              let
                event' = do
                  let add1 = map (add 1) event
                  let add2 = map (add 2) add1
                  let add3 = map (add 3) add2
                  let foldy = fold (\b a -> a + b) 0 add3
                  let add4 = map (add 4) add3
                  let altr = foldy <|> add2 <|> empty <|> add4 <|> empty
                  sampleOnRight add2 (map (\a b -> b /\ a) (filter (_ > 5) altr))
              u <- liftST $ subscribe event' \i ->
                liftST $ void $ STRef.modify (Array.cons i) r
              push 0
              liftST (STRef.read r) >>= shouldEqual [ Tuple 3 10, Tuple 3 6 ]
              liftST $ void $ STRef.write [] r
              push 0
              liftST (STRef.read r) >>= shouldEqual [ Tuple 3 10, Tuple 3 12 ]
              liftST $ void $ STRef.write [] r
              push 0
              liftST (STRef.read r) >>= shouldEqual [ Tuple 3 10, Tuple 3 18 ]
              liftST u
          it "should ignore left pushes on initial event but respond to both right pushes" $ liftEffect do
            let

              e :: forall a. Event a -> Event (Tuple Int Int)
              e e' = Tuple <$> (e' $> 1 <|> e' $> 2) <*> (e' $> 3 <|> e' $> 4)
            r <- liftST $ STRef.new []
            ep <- liftST $ Event.create
            u <- liftST $ subscribe (e ep.event) \i ->
              liftST $ void $ STRef.modify (flip Array.snoc i) r
            ep.push unit
            liftST (STRef.read r) >>= \y -> y `shouldEqual` [Tuple 2 3, Tuple 2 4]
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
          describe "Keep latest" do
            it "should emit when an event keeps itself" $ liftEffect do
              r <- liftST $ STRef.new []
              { push, event } <- liftST $ Event.create
              u1 <- liftST $ subscribe (keepLatest (event $> event)) \i ->
                liftST $ void $ STRef.modify (Array.cons i) r
              push 0
              v <- liftST $ STRef.read r
              v `shouldEqual` [ 0 ]
              liftST u1
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
                  memoize (identity (map fn event)) \e -> Event.makeEvent \k -> do
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
                  $ memoize event
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
              let b = stRefToBehavior r
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
