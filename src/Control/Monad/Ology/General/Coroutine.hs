module Control.Monad.Ology.General.Coroutine where

import Control.Monad.Ology.General.Trans.Trans
import Control.Monad.Ology.General.Trans.Tunnel
import Control.Monad.Ology.Specific.CoroutineT
import Control.Monad.Ology.Specific.StepT
import Import

-- | Monads in which one can do coroutines.
class Monad m => MonadCoroutine m where
    coroutineSuspend :: ((p -> m q) -> m r) -> CoroutineT p q m r

-- | Uses threads.
instance MonadCoroutine IO where
    coroutineSuspend :: ((p -> IO q) -> IO r) -> CoroutineT p q IO r
    coroutineSuspend action =
        MkStepT $ do
            invar <- newEmptyMVar
            outvar <- newEmptyMVar
            _ <-
                forkIO $ do
                    r <-
                        action $ \p -> do
                            putMVar outvar $
                                Right $
                                MkTurn p $ \q ->
                                    MkStepT $ do
                                        putMVar invar q
                                        takeMVar outvar
                            takeMVar invar
                    putMVar outvar $ Left r
            takeMVar outvar

instance (MonadTransTunnel t, MonadCoroutine m) => MonadCoroutine (t m) where
    coroutineSuspend call = underTunnelStepT $ \tun -> coroutineSuspend $ \pmq -> tun $ call $ \p -> lift $ pmq p

-- | A type synoynm for a common pattern for closing opened resources, e.g.
-- 'System.IO.withFile',
-- 'System.IO.withBinaryFile',
-- etc.
type With (m :: k -> Type) (t :: Type) = forall (r :: k). (t -> m r) -> m r

unpickWith ::
       forall m a. MonadCoroutine m
    => With m a
    -> m (a, m ())
unpickWith w = do
    etp <- unStepT $ coroutineSuspend w
    case etp of
        Left a -> return (a, return ())
        Right (MkTurn a f) -> return (a, fmap (\_ -> ()) $ runCoroutine $ f a)

pickWith ::
       forall m a. Monad m
    => m (a, m ())
    -> With m a
pickWith mac amr = do
    (a, closer) <- mac
    r <- amr a
    closer
    return r
