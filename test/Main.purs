module Test.Main where

import Prelude

import Control.Alt ((<|>))
import Control.Monad.ST (ST, run)
import Control.Monad.ST.Class (liftST)
import Control.Monad.ST.Global (Global, toEffect)
import Control.Monad.ST.Ref (STRef)
import Control.Monad.ST.Ref as STRef
import Control.Monad.ST.Uncurried (mkSTFn2, runSTFn2)
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
import Effect.Uncurried (mkEffectFn1, runEffectFn2)
import Effect.Unsafe (unsafePerformEffect)
import FRP.Behavior (ABehavior, Behavior, behavior, gate)
import FRP.Event (Backdoor, Event, EventIO, MakeEvent(..), Subscriber(..), backdoor, hot, keepLatest, mailboxed, makeEvent, makePureEvent, memoize, merge, sampleOnRight, subscribe)
import FRP.Event as Event
import FRP.Event.Class (fold)
import FRP.Event.Time (debounce, interval)
import FRP.Event.VBus (V, vbus)
import Test.Spec (describe, it)
import Test.Spec.Assertions (shouldEqual, shouldSatisfy)
import Test.Spec.Console (write)
import Test.Spec.Reporter (consoleReporter)
import Test.Spec.Runner (runSpec)
import Type.Proxy (Proxy(..))
import Unsafe.Coerce (unsafeCoerce)

foreign import unsafeBackdoor :: MakeEvent -> Backdoor -> Effect MakeEvent

refToBehavior :: Ref.Ref ~> Behavior
refToBehavior r = behavior \e -> makeEvent \k -> Event.subscribe e \f -> Ref.read r >>=
  (k <<< f)

stRefToBehavior :: STRef Global ~> ABehavior Event
stRefToBehavior r = behavior \e -> makeEvent \k -> Event.subscribe e \f ->
  liftST (STRef.read r) >>= (k <<< f)

modify__ :: forall a r. (a -> a) -> STRef r a -> ST r Unit
modify__ a b = void $ STRef.modify a b

fresh :: forall a r. a -> ST r (STRef r a)
fresh = STRef.new

type Test =
  V
    ( a :: Int
    , b :: Unit
    , c :: V (a :: Int, b :: String, q :: V (r :: Boolean))
    , d :: Array Int
    )

main :: Effect Unit
main = do
  launchAff_
    $ runSpec [ consoleReporter ] do
        describe "Event" do
          it "should do simple stuff" $ liftEffect do
            r <- toEffect $ STRef.new []
            unsubscribe <- subscribe ((pure 0)) \i ->
              liftST $ void $ STRef.modify (Array.cons i) r
            v <- toEffect $ STRef.read r
            unsubscribe
            v `shouldEqual` [ 0 ]
          it "should do complex stuff" $ liftEffect do
            r <- toEffect $ STRef.new []
            { push, event } <- Event.create
            u1 <- subscribe (event) \i ->
              liftST $ void $ STRef.modify (Array.cons i) r
            push 0
            v <- toEffect $ STRef.read r
            v `shouldEqual` [ 0 ]
            u2 <- subscribe (event) \i ->
              liftST $ void $ STRef.modify (Array.cons (negate i)) r
            v' <- toEffect $ STRef.read r
            v' `shouldEqual` [ 0 ]
            push 1
            v'' <- toEffect $ STRef.read r
            v'' `shouldEqual` [ -1, 1, 0 ]
            u1 *> u2
          it "should do a lot more complex addition" $ liftEffect do
            r <- toEffect $ STRef.new []
            let
              event = do
                let
                  add1 = map (add 1) (pure 0)
                  add2 = map (add 2) add1
                  add3 = map (add 3) add2
                  add4 = map (add 4) add3
                add1 <|> add4
            u <- subscribe event \i ->
              liftST $ void $ STRef.modify (Array.cons i) r
            v <- toEffect $ STRef.read r
            v `shouldEqual` [ 10, 1 ]
            u
          it "should handle alt" $ liftEffect do
            r <- toEffect $ STRef.new []
            let
              event = do
                let
                  add1 = (map (add 1) (pure 0))
                  add2 = map (add 2) add1
                  add3 = map (add 3) add2
                  add4 = map (add 4) add3
                  altr = add1 <|> add2 <|> empty <|> add4 <|> empty
                add1 <|> altr
            u <- subscribe event \i ->
              liftST $ void $ STRef.modify (Array.cons i) r
            v <- toEffect $ STRef.read r
            v `shouldEqual` [ 10, 3, 1, 1 ]
            u
          it "should handle filter 1" $ liftEffect do
            r <- toEffect $ STRef.new []
            let
              event = do
                let
                  add1 = map (add 1) (pure 0)
                  add2 = map (add 2) add1
                  add3 = map (add 3) add2
                  add4 = map (add 4) add3
                  altr = add1 <|> add2 <|> empty <|> add4 <|> empty
                  fm = (filter (_ < 5) altr)
                add1 <|> fm
            u <- subscribe event \i ->
              liftST $ void $ STRef.modify (Array.cons i) r
            v <- toEffect $ STRef.read r
            v `shouldEqual` [ 3, 1, 1 ]
            u
          it "should handle filter 2" $ liftEffect do
            r <- toEffect $ STRef.new []
            let add1 = (map (add 1) (pure 0))
            let add2 = map (add 2) add1
            let add3 = map (add 3) add2
            let add4 = map (add 4) add3
            let altr = add1 <|> add2 <|> empty <|> add4 <|> empty
            let fm = (filter (_ > 5) altr)
            u <- subscribe (add1 <|> fm) \i ->
              liftST $ void $ STRef.modify (Array.cons i) r
            v <- toEffect $ STRef.read r
            v `shouldEqual` [ 10, 1 ]
            u
          it "should handle fold 0" $ liftEffect do
            r <- toEffect $ STRef.new []
            { push, event } <- Event.create
            let
              event' = do
                let foldy = (fold (\b _ -> b + 1) 0 event)
                let add2 = map (add 2) foldy
                let add3 = map (add 3) add2
                let add4 = map (add 4) add3
                let altr = foldy <|> add2 <|> empty <|> add4 <|> empty
                let fm = (filter (_ > 5) altr)
                foldy <|> fm
            u <- subscribe event' \i ->
              liftST $ void $ STRef.modify (Array.cons i) r
            push unit
            toEffect (STRef.read r) >>= shouldEqual [ 10, 1 ]
            toEffect $ void $ STRef.write [] r
            push unit
            toEffect (STRef.read r) >>= shouldEqual [ 11, 2 ]
            toEffect $ void $ STRef.write [] r
            push unit
            toEffect (STRef.read r) >>= shouldEqual [ 12, 3 ]
            u
          it "should handle fold 1" do
            liftEffect do
              r <- toEffect $ STRef.new []
              { push, event } <- Event.create
              let
                event' = do
                  let add1 = map (add 1) event
                  let add2 = map (add 2) add1
                  let add3 = map (add 3) add2
                  let foldy = fold (\b a -> a + b) 0 add3
                  let add4 = map (add 4) add3
                  let altr = foldy <|> add2 <|> empty <|> add4 <|> empty
                  sampleOnRight add2 (map (\a b -> b /\ a) (filter (_ > 5) altr))
              u <- subscribe event' \i ->
                liftST $ void $ STRef.modify (Array.cons i) r
              push 0
              toEffect (STRef.read r) >>= shouldEqual [ Tuple 3 10, Tuple 3 6 ]
              toEffect $ void $ STRef.write [] r
              push 0
              toEffect (STRef.read r) >>= shouldEqual [ Tuple 3 10, Tuple 3 12 ]
              toEffect $ void $ STRef.write [] r
              push 0
              toEffect (STRef.read r) >>= shouldEqual [ Tuple 3 10, Tuple 3 18 ]
              u
          it "should match Applicative Array instance" $ liftEffect do
            let
              x :: Array (Tuple Int Int)
              x = Tuple <$> (pure 1 <|> pure 2) <*> (pure 3 <|> pure 4)

              e :: Event (Tuple Int Int)
              e = Tuple <$> (pure 1 <|> pure 2) <*> (pure 3 <|> pure 4)
            r <- toEffect $ STRef.new []
            u <- subscribe e \i ->
              liftST $ void $ STRef.modify (flip Array.snoc i) r
            toEffect (STRef.read r) >>= shouldEqual x
            u
          describe "Performance" do
            it "handles 10 subscriptions with a simple event and 1000 pushes" $ liftEffect do
              starts <- getTime <$> now
              r <- toEffect $ STRef.new []
              { push, event } <- Event.create
              us <- sequence $ replicate 10 $ subscribe (map (add 1) $ map (add 1) event) \i ->
                liftST $ void $ STRef.modify (Array.cons i) r
              for_ (replicate 1000 3) push
              sequence_ us
              ends <- getTime <$> now
              write ("Duration: " <> show (ends - starts) <> "\n")
            it "handles 1000 subscriptions with a simple event and 10 pushes" $ liftEffect do
              starts <- getTime <$> now
              r <- toEffect $ STRef.new []
              { push, event } <- Event.create
              us <- sequence $ replicate 1000 $ subscribe (map (add 1) $ map (add 1) event) \i ->
                liftST $ void $ STRef.modify (Array.cons i) r
              for_ (replicate 10 3) push
              sequence_ us
              ends <- getTime <$> now
              write ("Duration: " <> show (ends - starts) <> "\n")
            it "handles 1 subscription with a 10-nested event + 100 alts and 100 pushes" $ liftEffect do
              starts <- getTime <$> now
              r <- toEffect $ STRef.new []
              { push, event } <- Event.create
              let e = merge $ replicate 100 $ foldr ($) event (replicate 10 (map (add 1)))
              u <- subscribe e \i -> liftST $ void $ STRef.modify (Array.cons i) r
              for_ (replicate 100 3) push
              u
              ends <- getTime <$> now
              write ("Duration: " <> show (ends - starts) <> "\n")
            it "handles 1 subscription with a 10-nested event + array of 100 and 100 pushes" $ liftEffect do
              starts <- getTime <$> now
              r <- toEffect $ STRef.new []
              { push, event } <- Event.create
              let event' = map (replicate 100) $ foldr ($) event (replicate 10 (map (add 1)))
              u <- subscribe event' \i ->
                liftST $ void $ STRef.modify (Array.cons i) r
              for_ (replicate 100 3) push
              u
              ends <- getTime <$> now
              write ("Duration: " <> show (ends - starts) <> "\n")
          describe "Memoization" do
            it "should not memoize" $ liftEffect do
              { push, event } <- Event.create
              count <- Ref.new 0
              let
                fn v =
                  unsafePerformEffect do
                    Ref.modify_ (add 1) count
                    pure $ v
              let mapped = identity (map fn event)
              u1 <- Event.subscribe mapped (pure (pure unit))
              u2 <- Event.subscribe mapped (pure (pure unit))
              push 0
              Ref.read count >>= shouldEqual 2
              u1
              u2
            it "should memoize" $ liftEffect do
              { push, event } <- Event.create
              count <- Ref.new 0
              let
                fn v =
                  unsafePerformEffect do
                    Ref.modify_ (add 1) count
                    pure $ v
                mapped = keepLatest $
                  memoize (identity (map fn event)) \e -> Event.makeEvent \k -> do
                    u1 <- Event.subscribe e (\_ -> pure unit)
                    u2 <- Event.subscribe e k
                    pure (u1 *> u2)
              u <- Event.subscribe mapped (\_ -> pure unit)
              push 0
              Ref.read count >>= shouldEqual 1
              u
            it "should not memoize when applied internally" $ liftEffect do
              { push, event } <- Event.create
              count <- Ref.new 0
              let
                fn v =
                  unsafePerformEffect do
                    Ref.modify_ (add 1) count
                    pure $ v
                mapped = keepLatest
                  $ memoize event
                  $ (lcmap (identity <<< map fn)) \e ->
                      Event.makeEvent \k -> do
                        u1 <- Event.subscribe e (\_ -> pure unit)
                        u2 <- Event.subscribe e k
                        pure (u1 *> u2)
              u <- Event.subscribe mapped (\_ -> pure unit)
              push 0
              Ref.read count >>= shouldEqual 2
              u
          describe "Apply" do
            it "respects both sides of application" $ liftEffect do
              { event, push } <- Event.create
              rf0 <- toEffect $ STRef.new ""
              rf1 <- toEffect $ STRef.new ""
              void $ Event.subscribe ((append <$> pure "a") <*> event) (liftST <<< void <<< flip STRef.write rf0)
              void $ Event.subscribe ((append <$> event) <*> pure "b") (liftST <<< void <<< flip STRef.write rf1)
              push "c"
              rf0' <- toEffect $ STRef.read rf0
              rf1' <- toEffect $ STRef.read rf1
              rf0' `shouldEqual` "ac"
              rf1' `shouldEqual` "cb"
            it "always applies updates from left to right, emitting at each update" $ liftEffect do
              r <- toEffect $ STRef.new []
              { push, event } <- Event.create
              u <- Event.subscribe (let x = event in (map add x) <*> x) \i ->
                liftST $ void $ STRef.modify (flip Array.snoc i) r
              push 1
              push 2
              o <- toEffect $ STRef.read r
              o `shouldEqual` [ 2, 3, 4 ]
              u
            it "always applies multiple updates from left to right, emitting at each update" $ liftEffect do
              r <- toEffect $ STRef.new []
              { push, event } <- Event.create
              let addSixNums x y z a b c = x + y + z + a + b + c
              u <- Event.subscribe (let x = event in addSixNums <$> x <*> x <*> x <*> x <*> x <*> x) \i ->
                liftST $ void $ STRef.modify (flip Array.snoc i) r
              push 1
              push 2
              o <- toEffect $ STRef.read r
              o `shouldEqual` [ 6, 7, 8, 9, 10, 11, 12 ]
              u
          describe "Mailboxed" do
            it "should work" $ liftEffect do
              r <- toEffect $ STRef.new []
              e <- Event.create
              u <- Event.subscribe (keepLatest $ mailboxed e.event \f -> f 3 <|> f 4) \i ->
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
              o <- toEffect $ STRef.read r
              o `shouldEqual` [ false, false, true ]
              u
          describe "Mailbox" do
            it "should work" $ liftEffect do
              r <- toEffect $ STRef.new []
              e <- Event.mailbox
              u <- Event.subscribe (e.event 3 <|> e.event 4) \i ->
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
              o <- toEffect $ STRef.read r
              o `shouldEqual` [ false, false, true ]
              u
          describe "Gate" do
            it "should work" $ liftEffect do
              eio <- Event.create
              r <- toEffect $ STRef.new false
              n <- toEffect $ STRef.new 0
              let b = stRefToBehavior r
              _ <- Event.subscribe (gate b eio.event) \_ ->
                liftST $ void $ STRef.modify (add 1) n
              do
                eio.push unit
                eio.push unit
              toEffect $ void $ STRef.write true r
              do
                eio.push unit
                eio.push unit
                eio.push unit
              toEffect $ void $ STRef.write false r
              do
                eio.push unit
                eio.push unit
              res <- toEffect $ STRef.read n
              shouldEqual res 3
          describe "VBus" do
            it "works with simple pushing" $ liftEffect do
              r <- toEffect $ STRef.new []
              u <- Event.subscribe
                ( keepLatest $ vbus (Proxy :: _ Test)
                    ( \p e -> e.d <|> Event.makeEvent \k -> do
                        k [ 1, 2 ]
                        p.d [ 34 ]
                        pure (pure unit)
                    )
                )
                \i -> liftST $ void $ STRef.modify (append i) r
              u
              toEffect (STRef.read r) >>= shouldEqual [ 34, 1, 2 ]
            it "works with more complex pushing 1" $ liftEffect do
              r <- toEffect $ STRef.new ""
              u <- Event.subscribe
                ( keepLatest $ vbus (Proxy :: _ Test)
                    ( \p e -> map show e.d <|> map show e.c.a <|> map show e.c.q.r <|> Event.makeEvent \_ -> do
                        p.d [ 1 ]
                        p.c.a 55
                        p.c.q.r false
                        p.b unit
                        pure (pure unit)
                    )
                )
                \i -> liftST $ void $ STRef.modify (append i) r
              u
              toEffect (STRef.read r) >>= shouldEqual "false55[1]"
            it "works with more complex pushing 2" $ liftEffect do
              r <- toEffect $ STRef.new ""
              u <- Event.subscribe
                ( keepLatest $ vbus (Proxy :: _ Test)
                    ( \p e -> map show e.d <|> map show e.c.a <|> map show e.b <|> Event.makeEvent \_ -> do
                        p.d [ 1 ]
                        p.c.a 55
                        p.c.q.r false
                        p.b unit
                        pure (pure unit)
                    )
                )
                \i -> liftST $ void $ STRef.modify (append i) r
              u
              toEffect (STRef.read r) >>= shouldEqual "unit55[1]"

        describe "Miscellaneous" do
          describe "Hot" do
            it "should work" do
              r <- liftEffect $ Ref.new 0
              x <- liftEffect $ Ref.new 0
              let
                subs e = makeEvent \k -> do
                  Ref.modify_ (add 1) r
                  Event.subscribe e k
              { event, unsubscribe } <- liftEffect $ hot (subs (interval 50))
              u0 <- liftEffect $ Event.subscribe event \_ -> Ref.modify_ (add 1) x
              u1 <- liftEffect $ Event.subscribe event \_ -> Ref.modify_ (add 1) x
              delay (Milliseconds 800.0)
              liftEffect $ u0 *> u1 *> unsubscribe
              x' <- liftEffect $ Ref.read x
              r' <- liftEffect $ Ref.read r
              x' `shouldSatisfy` (_ > 10)
              r' `shouldEqual` 1
          describe "Fix" do
            it "should work" do
              { event, push } <- liftEffect Event.create
              rf <- liftEffect $ Ref.new []
              unsub <- liftEffect $ Event.subscribe (debounce (Milliseconds 1000.0) event) (\i -> Ref.modify_ (Array.cons i) rf)
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
                unsub
          describe "Purity" do
            it "Preserves purity when asked for, otherwise not" $ liftEffect do
              let event = (makeEvent \k -> k 1 *> (pure (pure unit))) <|> (makePureEvent \k -> k 42 *> (pure (pure unit))) <|> (makeEvent \k -> k 108 *> (pure (pure unit))) <|> (makePureEvent \k -> k 333 *> (pure (pure unit)))
              rf <- liftEffect $ Ref.new []
              unsub <- liftEffect $ Event.subscribe event (\i -> Ref.modify_ (Array.cons i) rf)
              o <- Ref.read rf
              o `shouldEqual` [ 333, 108, 42, 1 ]
              let
                oo = run do
                  rff <- STRef.new []
                  usu <- Event.subscribePure event (\i -> void $ STRef.modify (Array.cons i) rff)
                  usu
                  STRef.read rff
              oo `shouldEqual` [ 333, 42 ]
              unsub

          describe "Debounce" do
            it "debounces" do
              let
                f emitSecond = do
                  { event, push } <- liftEffect $ Event.create
                  rf <- liftEffect $ Ref.new []
                  unsub <- liftEffect $ Event.subscribe (debounce (Milliseconds 500.0) event) (\i -> Ref.modify_ (Array.cons i) rf)
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
                  liftEffect unsub
              f true
              f false

          describe "Backdoor" do
            it "should work" $ liftEffect do
              hack :: EventIO Int <- Event.create
              rf <- Ref.new []
              old <- unsafeBackdoor (MakeEvent \_ -> unsafeCoerce hack.event) backdoor
              let e0 = Event.makeEvent \k -> k 42 *> pure (pure unit)
              _ <- Event.subscribe e0 \i -> Ref.modify_ (Array.cons i) rf
              hack.push 1
              hack.push 2
              hack.push 3
              a <- Ref.read rf
              _ <- unsafeBackdoor old backdoor
              shouldEqual a [ 3, 2, 1 ]

          describe "Lemming" do
            it "follows like a lemming" $ liftEffect do
              ev :: EventIO Int <- Event.create
              rf <- Ref.new []
              let e0 = Event.makeLemmingEvent \s k -> s ev.event k
              _ <- Event.subscribe e0 \i -> Ref.modify_ (Array.cons i) rf
              ev.push 1
              ev.push 2
              ev.push 3
              a <- Ref.read rf
              shouldEqual a [ 3, 2, 1 ]

            it "follows like an optimized lemming" $ liftEffect do
              ev :: EventIO Int <- Event.create
              rf <- Ref.new []
              let e0 = Event.makeLemmingEventO (mkSTFn2 \(Subscriber s) k -> runSTFn2 s ev.event k)
              _ <- runEffectFn2 Event.subscribeO e0 (mkEffectFn1 \i -> Ref.modify_ (Array.cons i) rf)
              ev.push 1
              ev.push 2
              ev.push 3
              a <- Ref.read rf
              shouldEqual a [ 3, 2, 1 ]
