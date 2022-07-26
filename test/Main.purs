module Test.Main where

import Prelude

import Control.Alt ((<|>))
import Control.Monad.ST.Internal (ST, STRef, run)
import Control.Monad.ST.Internal as RRef
import Control.Monad.Trans.Class (lift)
import Control.Monad.Writer (execWriterT, tell)
import Control.Plus (empty)
import Data.Array (cons, replicate)
import Data.Filterable (filter)
import Data.JSDate (getTime, now)
import Data.Profunctor (lcmap)
import Data.Traversable (foldr, for_, oneOf, sequence)
import Data.Tuple (Tuple(..))
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Effect.Aff (Milliseconds(..), delay, launchAff_)
import Effect.Class (liftEffect)
import Effect.Ref as Ref
import Effect.Unsafe (unsafePerformEffect)
import FRP.Behavior (Behavior, behavior, gate)
import FRP.Event (hot, keepLatest, makeEvent, memoize, sampleOn, mailboxed)
import FRP.Event as Event
import FRP.Event.Class (class IsEvent, bang, fold)
import FRP.Event.Legacy as Legacy
import FRP.Event.Time (debounce, interval)
import FRP.Event.VBus (V, vbus)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual, shouldSatisfy)
import Test.Spec.Console (write)
import Test.Spec.Reporter (consoleReporter)
import Test.Spec.Runner (runSpec)
import Type.Proxy (Proxy(..))

refToBehavior :: Ref.Ref ~> Behavior
refToBehavior r = behavior \e -> makeEvent \k -> Event.subscribe e \f -> Ref.read r >>=
  (k <<< f)

modify__ :: forall a r. (a -> a) -> STRef r a -> ST r Unit
modify__ a b = void $ RRef.modify a b

fresh :: forall a r. a -> ST r (STRef r a)
fresh = RRef.new

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
        let
          suite
            :: forall event
             . IsEvent event
            => String
            -> (forall i o. event i -> (forall event'. IsEvent event' => event' i -> event' o) -> event o)
            -> (forall a. Effect { push :: a -> Effect Unit, event :: event a })
            -> (forall a. event a -> (a -> Effect Unit) -> Effect (Effect Unit))
            -> Spec Unit
          suite name context create subscribe =
            describe ("Testing " <> name) do
              it "should do simple stuff" do
                liftEffect do
                  rf <- Ref.new []
                  unsub <- subscribe (context (bang 0) identity) \i -> Ref.modify_ (cons i) rf
                  o <- Ref.read rf
                  o `shouldEqual` [ 0 ]
                  unsub
              it "should do complex stuff" do
                liftEffect do
                  rf <- Ref.new []
                  { push, event } <- create
                  unsub1 <- subscribe (context event identity) \i -> Ref.modify_ (cons i) rf
                  push 0
                  o <- Ref.read rf
                  o `shouldEqual` [ 0 ]
                  unsub2 <- subscribe (context event identity) \i -> Ref.modify_ (cons (negate i)) rf
                  o' <- Ref.read rf
                  o' `shouldEqual` [ 0 ]
                  push 1
                  o'' <- Ref.read rf
                  o'' `shouldEqual` [ -1, 1, 0 ]
                  unsub1 *> unsub2
              it "should do a lot more complex addition" do
                liftEffect do
                  rf <- Ref.new []
                  let
                    x = context (bang 0) \i ->
                      let
                        add1 = map (add 1) i
                        add2 = map (add 2) add1
                        add3 = map (add 3) add2
                        add4 = map (add 4) add3
                      in
                        add1 <|> add4
                  unsub <- subscribe x \i -> Ref.modify_ (cons i) rf
                  o <- Ref.read rf
                  o `shouldEqual` [ 10, 1 ]
                  unsub
              it "should handle alt" do
                liftEffect do
                  rf <- Ref.new []
                  let
                    x = context (bang 0) \i ->
                      let
                        add1 = (map (add 1) i)
                        add2 = map (add 2) add1
                        add3 = map (add 3) add2
                        add4 = map (add 4) add3
                        altr = add1 <|> add2 <|> empty <|> add4 <|> empty
                      in
                        add1 <|> altr
                  unsub <- subscribe x \i -> Ref.modify_ (cons i) rf
                  o <- Ref.read rf
                  o `shouldEqual` [ 10, 3, 1, 1 ]
                  unsub
              it "should handle filter 1" do
                liftEffect do
                  rf <- Ref.new []
                  let
                    x = context (bang 0) \i ->
                      let
                        add1 = map (add 1) i
                        add2 = map (add 2) add1
                        add3 = map (add 3) add2
                        add4 = map (add 4) add3
                        altr = add1 <|> add2 <|> empty <|> add4 <|> empty
                        fm = (filter (_ < 5) altr)
                      in
                        add1 <|> fm
                  unsub <- subscribe x (\i -> Ref.modify_ (cons i) rf)
                  o <- Ref.read rf
                  o `shouldEqual` [ 3, 1, 1 ]
                  unsub
              it "should handle filter 2" do
                liftEffect do
                  rf <- Ref.new []
                  let add1 = (map (add 1) (bang 0))
                  let add2 = map (add 2) add1
                  let add3 = map (add 3) add2
                  let add4 = map (add 4) add3
                  let altr = add1 <|> add2 <|> empty <|> add4 <|> empty
                  let fm = (filter (_ > 5) altr)
                  unsub <- subscribe (add1 <|> fm) (\i -> Ref.modify_ (cons i) rf)
                  o <- Ref.read rf
                  o `shouldEqual` [ 10, 1 ]
                  unsub
              it "should handle fold 0" do
                liftEffect do
                  rf <- Ref.new []
                  { push, event } <- create
                  let
                    x = context event \i -> do
                      let foldy = (fold (\_ b -> b + 1) i 0)
                      let add2 = map (add 2) foldy
                      let add3 = map (add 3) add2
                      let add4 = map (add 4) add3
                      let altr = foldy <|> add2 <|> empty <|> add4 <|> empty
                      let fm = (filter (_ > 5) altr)
                      foldy <|> fm
                  unsub <- subscribe x (\i -> Ref.modify_ (cons i) rf)
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
                liftEffect do
                  rf <- Ref.new []
                  { push, event } <- create
                  let
                    x = context event \i -> do
                      let add1 = map (add 1) i
                      let add2 = map (add 2) add1
                      let add3 = map (add 3) add2
                      let foldy = fold (\a b -> a + b) add3 0
                      let add4 = map (add 4) add3
                      let altr = foldy <|> add2 <|> empty <|> add4 <|> empty
                      sampleOn add2 (map (\a b -> b /\ a) (filter (_ > 5) altr))
                  unsub <- subscribe x (\i -> Ref.modify_ (cons i) rf)
                  push 0
                  Ref.read rf >>= shouldEqual [ Tuple 3 10, Tuple 3 6 ]
                  Ref.write [] rf
                  push 0
                  Ref.read rf >>= shouldEqual [ Tuple 3 10, Tuple 3 12 ]
                  Ref.write [] rf
                  push 0
                  Ref.read rf >>= shouldEqual [ Tuple 3 10, Tuple 3 18 ]
                  unsub
        suite "Event" (\i f -> f i) Event.create Event.subscribe
        suite "Legacy" (\i f -> f i) Legacy.create Legacy.subscribe
        let
          performanceSuite
            :: forall event
             . IsEvent event
            => String
            -> (forall i o. event i -> (forall event'. IsEvent event' => event' i -> event' o) -> event o)
            -> (forall a. Effect { push :: a -> Effect Unit, event :: event a })
            -> (forall a. event a -> (a -> Effect Unit) -> Effect (Effect Unit))
            -> Spec Unit
          performanceSuite name context create subscribe =
            describe ("Performance testing " <> name) do
              it "handles 10 subscriptions with a simple event and 1000 pushes" do
                liftEffect do
                  starts <- getTime <$> now
                  rf <- Ref.new []
                  { push, event } <- create
                  unsubs <- sequence $ replicate 10 (subscribe (context event (\i -> map (add 1) $ map (add 1) i)) \i -> Ref.modify_ (cons i) rf)
                  for_ (replicate 1000 3) \i -> push i
                  for_ unsubs \unsub -> unsub
                  ends <- getTime <$> now
                  write ("Duration: " <> show (ends - starts) <> "\n")
              it "handles 1000 subscriptions with a simple event and 10 pushes" do
                liftEffect do
                  starts <- getTime <$> now
                  rf <- Ref.new []
                  { push, event } <- create
                  unsubs <- sequence $ replicate 1000 (subscribe (context event (\i -> map (add 1) $ map (add 1) i)) \i -> Ref.modify_ (cons i) rf)
                  for_ (replicate 10 3) \i -> push i
                  for_ unsubs \unsub -> unsub
                  ends <- getTime <$> now
                  write ("Duration: " <> show (ends - starts) <> "\n")
              it "handles 10 subscriptions with a 100-nested event and 100 pushes" do
                liftEffect do
                  starts <- getTime <$> now
                  rf <- Ref.new []
                  { push, event } <- create
                  let e = context event (\i -> foldr ($) i (replicate 100 (map (add 1))))
                  unsubs <- sequence $ replicate 10 (subscribe e \i -> Ref.modify_ (cons i) rf)
                  for_ (replicate 100 3) \i -> push i
                  for_ unsubs \unsub -> unsub
                  ends <- getTime <$> now
                  write ("Duration: " <> show (ends - starts))
              it "handles 1 subscription with a 10-nested event + 100 alts and 100 pushes" do
                liftEffect do
                  starts <- getTime <$> now
                  rf <- Ref.new []
                  { push, event } <- create
                  let e = context event (\i -> oneOf $ replicate 100 $ foldr ($) i (replicate 10 (map (add 1))))
                  unsub <- subscribe e \i -> Ref.modify_ (cons i) rf
                  for_ (replicate 100 3) \i -> push i
                  unsub
                  ends <- getTime <$> now
                  write ("Duration: " <> show (ends - starts) <> "\n")
              it "handles 1 subscription with a 10-nested event + array of 100 and 100 pushes" do
                liftEffect do
                  starts <- getTime <$> now
                  rf <- Ref.new []
                  { push, event } <- create
                  let e = context event (\i -> map (replicate 100) $ foldr ($) i (replicate 10 (map (add 1))))
                  unsub <- subscribe e \i -> Ref.modify_ (cons i) rf
                  for_ (replicate 100 3) \i -> push i
                  unsub
                  ends <- getTime <$> now
                  write ("Duration: " <> show (ends - starts) <> "\n")
        performanceSuite "Event" (\i f -> f i) Event.create Event.subscribe
        performanceSuite "Legacy" (\i f -> f i) Legacy.create Legacy.subscribe
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
              let
                mapped = keepLatest $
                  memoize (identity (map fn event)) \e -> Event.makeEvent \k -> do
                    unsub1 <- Event.subscribe e mempty
                    unsub2 <- Event.subscribe e k
                    pure (unsub1 *> unsub2)

              usu <- Event.subscribe mapped (mempty)
              push 0
              Ref.read count >>= shouldEqual 1
              usu
          it "should not memoize when applied internally" do
            liftEffect do
              { push, event } <- Event.create
              count <- Ref.new 0
              let
                fn v =
                  unsafePerformEffect do
                    Ref.modify_ (add 1) count
                    pure $ v
              let
                mapped = keepLatest
                  $ memoize event
                  $ (lcmap (identity <<< map fn)) \e ->
                      Event.makeEvent \k -> do
                        unsub1 <- Event.subscribe e mempty
                        unsub2 <- Event.subscribe e k
                        pure (unsub1 *> unsub2)

              usu <- Event.subscribe mapped (mempty)
              push 0
              Ref.read count >>= shouldEqual 2
              usu
        describe "Hot" do
          it "is hot" do
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
        describe "Legacy" do
          it "has a somewhat puzzling result when it adds itself to itself (2 + 2 = 3)" $ liftEffect do
            rf <- Ref.new []
            { push, event } <- Legacy.create
            unsub <- Legacy.subscribe (let x = event in (map add x) <*> x) \i -> Ref.modify_ (cons i) rf
            push 2
            push 1
            o <- Ref.read rf
            o `shouldEqual` [ 2, 3, 4 ]
        describe "VBus" do
          it "works with simple pushing" $ liftEffect do
            r <- Ref.new []
            u <- Event.subscribe
              ( keepLatest $ vbus (Proxy :: _ Test)
                  ( \p e -> e.d <|> Event.makeEvent \k -> do
                      k [ 1, 2 ]
                      p.d [ 34 ]
                      pure (pure unit)
                  )
              )
              \i -> Ref.modify_ (append i) r
            u
            Ref.read r >>= shouldEqual [ 34, 1, 2 ]
          it "works with more complex pushing 1" $ liftEffect do
            r <- Ref.new ""
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
              \i -> Ref.modify_ (append i) r
            u
            Ref.read r >>= shouldEqual "false55[1]"
          it "works with more complex pushing 2" $ liftEffect do
            r <- Ref.new ""
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
              \i -> Ref.modify_ (append i) r
            u
            Ref.read r >>= shouldEqual "unit55[1]"
        describe "fix" do
          it "should work" do
            { event, push } <- liftEffect Event.create
            rf <- liftEffect $ Ref.new []
            unsub <- liftEffect $ Event.subscribe (debounce (Milliseconds 1000.0) event) (\i -> Ref.modify_ (cons i) rf)
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
        --------
        -- for st, we can't use the suite, as it leask a variable
        -- for now, copied and pasted below
        describe ("Testing ST") do
          it "should do simple stuff" do
            run
              ( execWriterT do
                  rf <- lift (fresh [])
                  unsub <- lift (Event.subscribe (bang 0) \i -> modify__ (cons i) rf)
                  o <- lift (RRef.read rf)
                  tell (o `shouldEqual` [ 0 ])
                  lift unsub
              )
          it "should do complex stuff" do
            run
              ( execWriterT do
                  rf <- lift $ fresh []
                  { push, event } <- lift $ Event.create
                  unsub1 <- lift $ Event.subscribe (event) \i -> modify__ (cons i) rf
                  lift $ push 0
                  o <- lift $ RRef.read rf
                  tell (o `shouldEqual` [ 0 ])
                  unsub2 <- lift $ Event.subscribe (event) \i -> modify__ (cons (negate i)) rf
                  o' <- lift $ RRef.read rf
                  tell (o' `shouldEqual` [ 0 ])
                  lift $ push 1
                  o'' <- lift $ RRef.read rf
                  tell (o'' `shouldEqual` [ -1, 1, 0 ])
                  lift $ unsub1 *> unsub2
              )
          it "should do a lot more complex addition" do
            run
              ( execWriterT do
                  rf <- lift $ fresh []
                  let
                    x = (bang 0) # \i ->
                      let
                        add1 = map (add 1) i
                        add2 = map (add 2) add1
                        add3 = map (add 3) add2
                        add4 = map (add 4) add3
                      in
                        add1 <|> add4
                  unsub <- lift $ Event.subscribe x \i -> modify__ (cons i) rf
                  o <- lift $ RRef.read rf
                  tell (o `shouldEqual` [ 10, 1 ])
                  lift $ unsub
              )
          it "should handle alt" do
            run
              ( execWriterT do
                  rf <- lift $ fresh []
                  let
                    x = (bang 0) # \i ->
                      let
                        add1 = (map (add 1) i)
                        add2 = map (add 2) add1
                        add3 = map (add 3) add2
                        add4 = map (add 4) add3
                        altr = add1 <|> add2 <|> empty <|> add4 <|> empty
                      in
                        add1 <|> altr
                  unsub <- lift $ Event.subscribe x \i -> modify__ (cons i) rf
                  o <- lift $ RRef.read rf
                  tell (o `shouldEqual` [ 10, 3, 1, 1 ])
                  lift $ unsub
              )
          it "should handle filter 1" do
            run
              ( execWriterT do
                  rf <- lift $ fresh []
                  let
                    x = (bang 0) # \i ->
                      let
                        add1 = map (add 1) i
                        add2 = map (add 2) add1
                        add3 = map (add 3) add2
                        add4 = map (add 4) add3
                        altr = add1 <|> add2 <|> empty <|> add4 <|> empty
                        fm = (filter (_ < 5) altr)
                      in
                        add1 <|> fm
                  unsub <- lift $ Event.subscribe x (\i -> modify__ (cons i) rf)
                  o <- lift $ RRef.read rf
                  tell (o `shouldEqual` [ 3, 1, 1 ])
                  lift $ unsub
              )
          it "should handle filter 2" do
            run
              ( execWriterT do
                  rf <- lift $ fresh []
                  let add1 = (map (add 1) (bang 0))
                  let add2 = map (add 2) add1
                  let add3 = map (add 3) add2
                  let add4 = map (add 4) add3
                  let altr = add1 <|> add2 <|> empty <|> add4 <|> empty
                  let fm = (filter (_ > 5) altr)
                  unsub <- lift $ Event.subscribe (add1 <|> fm) (\i -> modify__ (cons i) rf)
                  o <- lift $ RRef.read rf
                  tell (o `shouldEqual` [ 10, 1 ])
                  lift $ unsub
              )
          it "should handle fold 0" do
            run
              ( execWriterT do
                  rf <- lift $ fresh []
                  { push, event } <- lift $ Event.create
                  let
                    x = event # \i -> do
                      let foldy = (fold (\_ b -> b + 1) i 0)
                      let add2 = map (add 2) foldy
                      let add3 = map (add 3) add2
                      let add4 = map (add 4) add3
                      let altr = foldy <|> add2 <|> empty <|> add4 <|> empty
                      let fm = (filter (_ > 5) altr)
                      foldy <|> fm
                  unsub <- lift $ Event.subscribe x (\i -> modify__ (cons i) rf)
                  lift $ push unit
                  (lift $ RRef.read rf) >>= (tell <<< shouldEqual [ 10, 1 ])
                  void $ lift $ RRef.write [] rf
                  lift $ push unit
                  (lift $ RRef.read rf) >>= (tell <<< shouldEqual [ 11, 2 ])
                  lift $ void $ RRef.write [] rf
                  lift $ push unit
                  (lift $ RRef.read rf) >>= (tell <<< shouldEqual [ 12, 3 ])
                  lift $ unsub
              )
          it "should handle fold 1" do
            run
              ( execWriterT do
                  rf <- lift $ fresh []
                  { push, event } <- lift $ Event.create
                  let
                    x = event # \i -> do
                      let add1 = map (add 1) i
                      let add2 = map (add 2) add1
                      let add3 = map (add 3) add2
                      let foldy = fold (\a b -> a + b) add3 0
                      let add4 = map (add 4) add3
                      let altr = foldy <|> add2 <|> empty <|> add4 <|> empty
                      sampleOn add2 (map (\a b -> b /\ a) (filter (_ > 5) altr))
                  unsub <- lift $ Event.subscribe x (\i -> modify__ (cons i) rf)
                  lift $ push 0
                  (lift $ RRef.read rf) >>= tell <<< shouldEqual [ Tuple 3 10, Tuple 3 6 ]
                  lift $ void $ RRef.write [] rf
                  lift $ push 0
                  (lift $ RRef.read rf) >>= tell <<< shouldEqual [ Tuple 3 10, Tuple 3 12 ]
                  lift $ void $ RRef.write [] rf
                  lift $ push 0
                  (lift $ RRef.read rf) >>= tell <<< shouldEqual [ Tuple 3 10, Tuple 3 18 ]
                  lift $ unsub
              )
        describe "Mailboxed" do
          it "mailboxes" $ liftEffect do
            rf <- Ref.new []
            e <- Event.create
            unsub <- Event.subscribe (keepLatest $ mailboxed e.event \f -> f 3 <|> f 4) \i -> Ref.modify_ (cons i) rf
            e.push 42
            e.push 43
            e.push 44
            e.push 3 --
            e.push 42
            e.push 43
            e.push 44
            e.push 4 --
            e.push 42
            e.push 43
            e.push 3 --
            e.push 101
            o <- Ref.read rf
            o `shouldEqual` [ 3, 4, 3 ]
            unsub
        describe "Gate" do
          it "gates" $ liftEffect do
            eio <- Event.create
            rf <- Ref.new false
            n <- Ref.new 0
            let b = refToBehavior rf
            _ <- Event.subscribe (gate b eio.event) \_ -> Ref.modify_ (add 1) n
            eio.push unit
            eio.push unit
            Ref.write true rf
            eio.push unit
            eio.push unit
            eio.push unit
            Ref.write false rf
            eio.push unit
            eio.push unit
            res <- Ref.read n
            shouldEqual res 3
