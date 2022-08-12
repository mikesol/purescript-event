module FRP.Event.VBus (V, vbus, class VBus, VbusT, Vbus(..), vb, vbackdoor, VBackdoor) where

import Prelude

import Control.Monad.ST.Class (class MonadST)
import Data.Symbol (class IsSymbol)
import FRP.Event (AnEvent, makeEvent)
import Prim.Row as R
import Prim.RowList (class RowToList, RowList)
import Prim.RowList as RL
import Record as Record
import Type.Proxy (Proxy(..))
import Unsafe.Coerce (unsafeCoerce)

class VBus :: RowList Type -> Row Type -> Row Type -> Row Type -> Constraint
class VBus ri p e u | ri -> p e u where
  vb :: Proxy ri -> Proxy p -> Proxy e -> V u

instance vbusNil :: VBus RL.Nil () () () where
  vb _ _ _ = (unsafeCoerce :: {} -> V ()) {}

foreign import unsafeMarkAsVbus :: forall a. a -> a

data V (bus :: Row Type)

instance vbusCons1 ::
  ( IsSymbol key
  , RowToList i irl
  , R.Cons key { | p'' } p' p
  , R.Cons key { | e'' } e' e
  , VBus irl p'' e'' i
  , VBus rest p' e' u'
  , R.Cons key (V i) u' u
  , R.Lacks key p'
  , R.Lacks key e'
  , R.Lacks key u'
  ) =>
  VBus (RL.Cons key (V i) rest) p e u where
  vb _ _ _ = (unsafeCoerce :: { | u } -> V u) $ Record.insert
    (Proxy :: _ key)
    ( unsafeMarkAsVbus
        ( vb (Proxy :: _ irl)
            (Proxy :: _ p'')
            (Proxy :: _ e'')
        )
    )
    ( (unsafeCoerce :: V u' -> { | u' }) $
        ( vb (Proxy :: _ rest)
            (Proxy :: _ p')
            (Proxy :: _ e')
        )
    )

else instance vbusCons2 ::
  ( IsSymbol key
  , R.Cons key (z -> m Unit) p' p
  , R.Cons key (AnEvent m z) e' e
  , VBus rest p' e' u'
  , R.Cons key z u' u
  , R.Lacks key p'
  , R.Lacks key e'
  , R.Lacks key u'
  ) =>
  VBus (RL.Cons key z rest) p e u where
  vb _ _ _ = (unsafeCoerce :: { | u } -> V u) $ Record.insert
    (Proxy :: _ key)
    ((unsafeCoerce :: Unit -> z) unit)
    ( (unsafeCoerce :: V u' -> { | u' }) $
        ( vb (Proxy :: _ rest)
            (Proxy :: _ p')
            (Proxy :: _ e')
        )
    )

data S

foreign import unsafeDestroyS :: forall m. S -> m Unit

foreign import unsafePE
  :: forall m u p e
   . V u
  -> m { p :: { | p }, e :: { | e }, s :: S }

vbus :: VbusT
vbus i = (\(Vbus nt) -> nt) vbackdoor.vbus i

type VbusT =
  forall proxy ri i s m p e o u
   . RowToList i ri
  => MonadST s m
  => VBus ri p e u
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
          :: forall proxy ri i s m p e o u
           . RowToList i ri
          => MonadST s m
          => VBus ri p e u
          => proxy (V i)
          -> ({ | p } -> { | e } -> o)
          -> AnEvent m o
        vbus__ _ f = makeEvent \k -> do
          upe <- unsafePE vbd
          k (f upe.p upe.e)
          pure (unsafeDestroyS upe.s)
          where
          vbd = vb (Proxy :: _ ri) (Proxy :: _ p) (Proxy :: _ e)

        vbus_ :: Vbus
        vbus_ = Vbus vbus__
      in
        vbus_
  }