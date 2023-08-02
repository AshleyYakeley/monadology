module Control.Monad.Ology.General.Trans.Tunnel where

import Control.Monad.Ology.General.Function
import Control.Monad.Ology.General.IO
import Control.Monad.Ology.General.Inner
import Control.Monad.Ology.General.Trans.Constraint
import Control.Monad.Ology.General.Trans.Hoist
import Control.Monad.Ology.Specific.ComposeInner
import Import

-- | Monad transformers that allow \"tunneling\" (working with the monad under the transformer).
type MonadTransTunnel :: TransKind -> Constraint
class (MonadTransHoist t, MonadInner (Tunnel t)) => MonadTransTunnel t where
    -- | The tunnel monad of this transformer.
    type Tunnel t :: Type -> Type
    tunnel ::
           forall m r. Monad m
        => ((forall m1 a. Monad m1 => t m1 a -> m1 (Tunnel t a)) -> m (Tunnel t r))
        -> t m r

tunnelHoist ::
       forall t m1 m2. (MonadTransTunnel t, Monad m1, Monad m2)
    => (m1 --> m2)
    -> t m1 --> t m2
tunnelHoist mma sm1 = tunnel $ \tun -> mma $ tun sm1

backHoist :: (MonadTransTunnel t, Monad ma, Monad mb) => (ma -/-> mb) -> t ma -/-> t mb
backHoist wt tm = tunnel $ \unlift -> wt $ \tba -> unlift $ tm $ hoist tba

wBackHoist :: (MonadTransTunnel t, Monad ma, Monad mb) => WBackraised ma mb -> WBackraised (t ma) (t mb)
wBackHoist (MkWBackraised f) = MkWBackraised $ backHoist f

-- | Commute two transformers in a transformer stack, by commuting their tunnel monads.
commuteTWith ::
       forall ta tb m. (MonadTransTunnel ta, MonadTransTunnel tb, Monad m)
    => (forall r. Tunnel tb (Tunnel ta r) -> Tunnel ta (Tunnel tb r))
    -> ta (tb m) --> tb (ta m)
commuteTWith commutef tabm =
    case hasTransConstraint @Monad @ta @m of
        Dict ->
            case hasTransConstraint @Monad @tb @m of
                Dict -> tunnel $ \unliftb -> tunnel $ \unlifta -> fmap commutef $ unliftb $ unlifta tabm

-- | Commute two transformers in a transformer stack.
commuteT ::
       forall ta tb m. (MonadTransTunnel ta, MonadTransTunnel tb, Monad m)
    => ta (tb m) --> tb (ta m)
commuteT = commuteTWith commuteInner

commuteTBack ::
       forall ta tb m. (MonadTransTunnel ta, MonadTransTunnel tb, Monad m)
    => ta (tb m) -/-> tb (ta m)
commuteTBack call = commuteT $ call commuteT

instance MonadInner inner => MonadTransTunnel (ComposeInner inner) where
    type Tunnel (ComposeInner inner) = inner
    tunnel call = MkComposeInner $ call unComposeInner

class (MonadHoistIO m, MonadInner (TunnelIO m)) => MonadTunnelIO m where
    type TunnelIO m :: Type -> Type
    tunnelIO :: forall r. ((forall a. m a -> IO (TunnelIO m a)) -> IO (TunnelIO m r)) -> m r

instance MonadTunnelIO IO where
    type TunnelIO IO = Identity
    tunnelIO call = fmap runIdentity $ call $ \ma -> fmap Identity $ ma

instance (MonadTransTunnel t, MonadTunnelIO m, MonadIO (t m)) => MonadTunnelIO (t m) where
    type TunnelIO (t m) = ComposeInner (Tunnel t) (TunnelIO m)
    tunnelIO call =
        tunnel $ \unlift -> tunnelIO $ \unliftIO -> fmap unComposeInner $ call $ fmap MkComposeInner . unliftIO . unlift

instance (MonadTransTunnel t, TransConstraint MonadIO t) => TransConstraint MonadTunnelIO t where
    hasTransConstraint = withTransConstraintDict @MonadIO Dict

-- | for use in 'WUnlift', etc.
class (MonadTunnelIO m, MonadInner (TunnelIO m)) => MonadTunnelIOInner m

instance (MonadTunnelIO m, MonadInner (TunnelIO m)) => MonadTunnelIOInner m

instance (MonadTransTunnel t, TransConstraint MonadIO t) => TransConstraint MonadTunnelIOInner t where
    hasTransConstraint = withTransConstraintDict @MonadIO Dict
