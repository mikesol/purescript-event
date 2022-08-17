module FRP.Event.VBus (V, vbus, class VBus, VbusT, Vbus(..), vb, vbackdoor, VBackdoor) where

import Prelude

import Control.Monad.ST.Class (class MonadST)
import Data.Symbol (class IsSymbol)
import Data.Tuple.Nested (type (/\), (/\))
import FRP.Event (AnEvent, createHelper, makeEvent)
import Prim.Row as R
import Prim.RowList (class RowToList, RowList)
import Prim.RowList as RL
import Record as Record
import Type.Proxy (Proxy(..))

class VBus :: RowList Type -> Row Type -> Row Type -> (Type -> Type) -> Constraint
class VBus ri p e m | ri -> p e where
  vb :: forall s. MonadST s m => Proxy ri -> Proxy s -> Proxy m -> m ({ | p } /\ { | e })

instance vbusNil :: VBus RL.Nil () () m where
  vb _ _ _ = pure ({} /\ {})


data V (bus :: Row Type)

instance vbusCons1 ::
  ( IsSymbol key
  , RowToList i irl
  , R.Cons key { | p'' } p' p
  , R.Cons key { | e'' } e' e
  , VBus irl p'' e'' m
  , VBus rest p' e' m
  , R.Lacks key p'
  , R.Lacks key e'
  ) =>
  VBus (RL.Cons key (V i) rest) p e m where
  vb _ s m = do
    p /\ e <- vb (Proxy :: _ rest) s m 
    p' /\ e' <- vb (Proxy :: _ irl) s m
    pure (Record.insert (Proxy :: _ key) p' p /\ Record.insert (Proxy :: _ key) e' e)

else instance vbusCons2 ::
  ( IsSymbol key
  , R.Cons key (z -> m Unit) p' p
  , R.Cons key (AnEvent m z) e' e
  , VBus rest p' e' m
  , R.Lacks key p'
  , R.Lacks key e'
  ) =>
  VBus (RL.Cons key z rest) p e m where
  vb _ s m = do
    p /\ e <- vb (Proxy :: _ rest) s m
    { event, push } <- createHelper s m m
    pure (Record.insert (Proxy :: _ key) (push :: z -> m Unit) p /\ Record.insert (Proxy :: _ key) event e)


vbus :: VbusT
vbus i = (\(Vbus nt) -> nt) vbackdoor.vbus i

type VbusT =
  forall proxy ri i s m p e o
   . RowToList i ri
  => MonadST s m
  => VBus ri p e m
  => proxy (V i)
  -> ({ | p } -> { | e } -> o)
  -> AnEvent m o

newtype Vbus = Vbus VbusT

type VBackdoor = { vbus :: Vbus }

vbackdoor :: VBackdoor
vbackdoor =
  { vbus:
      let
        vbus__
          :: forall proxy ri i s m p e o
           . RowToList i ri
          => MonadST s m
          => VBus ri p e m
          => proxy (V i)
          -> ({ | p } -> { | e } -> o)
          -> AnEvent m o
        vbus__ _ f = makeEvent \k -> do
          e /\ p <- vb (Proxy :: _ ri) (Proxy :: _ s) (Proxy :: _ m)
          k (f e p)
          -- is any unsubscribe needed here?
          pure (pure unit)

        vbus_ :: Vbus
        vbus_ = Vbus vbus__
      in
        vbus_
  }