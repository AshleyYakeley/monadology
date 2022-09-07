module Control.Monad.Ology.General.Coroutine where

import Control.Monad.Ology.General.Trans.Hoist
import Control.Monad.Ology.General.Trans.Trans
import Control.Monad.Ology.General.Trans.Tunnel
import Control.Monad.Ology.General.Trans.Unlift
import Control.Monad.Ology.Specific.CoroutineT
import Control.Monad.Ology.Specific.StepT
import Import

class Monad m => MonadCoroutine m where
    coroutineSuspend :: ((p -> m q) -> m r) -> CoroutineT p q m r

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

instance (MonadTransUnlift t, MonadCoroutine m, MonadTunnelIOInner m, Monad (t m)) => MonadCoroutine (t m) where
    coroutineSuspend call =
        MkStepT $
        liftWithUnlift $ \unlift ->
            (fmap $ fmap $ fmap $ hoist lift) $
            singleStep $ coroutineSuspend $ \pmq -> unlift $ call $ \p -> lift $ pmq p

-- | A type synoynm for a common pattern for closing opened resources, e.g.
-- 'System.IO.withFile',
-- 'System.IO.withBinaryFile',
-- etc.
type With (m :: Type -> Type) (t :: Type) = forall (r :: Type). (t -> m r) -> m r

unpickWith ::
       forall m a. MonadCoroutine m
    => With m a
    -> m (a, m ())
unpickWith w = do
    etp <- singleStep $ coroutineSuspend w
    case etp of
        Left a -> return (a, return ())
        Right (MkTurn a f) -> return (a, fmap (\_ -> ()) $ runCoroutine $ f a)
