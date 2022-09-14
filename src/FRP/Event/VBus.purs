module FRP.Event.VBus (V, vbus, class VBus, VbusT, Vbus(..), vb, vbackdoor, VBackdoor) where

import Prelude

import Control.Monad.ST (ST)
import Control.Monad.ST.Global (Global)
import Data.Symbol (class IsSymbol)
import Data.Tuple.Nested (type (/\), (/\))
import Effect (Effect)
import FRP.Event (Event, create, makeLemmingEvent)
import Prim.Row as R
import Prim.RowList (class RowToList, RowList)
import Prim.RowList as RL
import Record as Record
import Type.Proxy (Proxy(..))
import Unsafe.Coerce (unsafeCoerce)

class VBus :: RowList Type -> Row Type -> Row Type -> Constraint
class VBus ri p e | ri -> p e where
  vb :: Proxy ri -> ST Global ({ | p } /\ { | e })

instance vbusNil :: VBus RL.Nil () () where
  vb _ = pure ({} /\ {})

data V (bus :: Row Type)

instance vbusCons1 ::
  ( IsSymbol key
  , RowToList i irl
  , R.Cons key { | p'' } p' p
  , R.Cons key { | e'' } e' e
  , VBus irl p'' e''
  , VBus rest p' e'
  , R.Lacks key p'
  , R.Lacks key e'
  ) =>
  VBus (RL.Cons key (V i) rest) p e where
  vb _ = do
    p /\ e <- vb (Proxy :: _ rest)
    p' /\ e' <- vb (Proxy :: _ irl)
    pure (Record.insert (Proxy :: _ key) p' p /\ Record.insert (Proxy :: _ key) e' e)

else instance vbusCons2 ::
  ( IsSymbol key
  , R.Cons key (z -> Effect Unit) p' p
  , R.Cons key (Event z) e' e
  , VBus rest p' e'
  , R.Lacks key p'
  , R.Lacks key e'
  ) =>
  VBus (RL.Cons key z rest) p e where
  vb _ = do
    p /\ e <- vb (Proxy :: _ rest)
    { event, push } <- (unsafeCoerce :: Effect _ -> ST Global _) create
    pure (Record.insert (Proxy :: _ key) push p /\ Record.insert (Proxy :: _ key) event e)

vbus :: VbusT
vbus i = (\(Vbus nt) -> nt) vbackdoor.vbus i

type VbusT =
  forall proxy ri i p e o
   . RowToList i ri
  => VBus ri p e
  => proxy (V i)
  -> ({ | p } -> { | e } -> o)
  -> Event o

newtype Vbus = Vbus VbusT

type VBackdoor = { vbus :: Vbus }

vbackdoor :: VBackdoor
vbackdoor =
  { vbus:
      let
        vbus__
          :: forall proxy ri i p e o
           . RowToList i ri
          => VBus ri p e
          => proxy (V i)
          -> ({ | p } -> { | e } -> o)
          -> Event o
        vbus__ _ f = makeLemmingEvent \_ k -> do
          e /\ p <- vb (Proxy :: _ ri)
          k (f e p)
          -- is any unsubscribe needed here?
          pure (pure unit)

        vbus_ :: Vbus
        vbus_ = Vbus vbus__
      in
        vbus_
  }