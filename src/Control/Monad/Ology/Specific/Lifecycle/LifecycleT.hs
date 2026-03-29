module Control.Monad.Ology.Specific.Lifecycle.LifecycleT
    ( LifecycleT (..)
    , Lifecycle
    , runLifecycle
    , lifecycleOnCloseIO
    , lifecycleOnClose
    , lifecycleGetCloser
    , forkLifecycle
    , lifecycleMonitor

      -- * With
    , With
    , withLifecycle
    , lifecycleWith

      -- * LifeState
    , LifeState
    , pattern NoLifeState
    , lifeStateModify
    , closeLifeState
    , getLifeState
    , addLifeState
    , modifyLifeState
    )
where

import Control.Monad.Ology.General
import Control.Monad.Ology.Specific.Durable
import Control.Monad.Ology.Specific.Lifecycle.LifeState
import Control.Monad.Ology.Specific.WriterT
import Import

-- | This is for managing the automatic closing of opened resources.
newtype LifecycleT (m :: Type -> Type) (a :: Type) = MkLifecycleT
    { unLifecycleT :: DurableWriterT (LifeState IO) m a
    }
    deriving newtype
        ( Functor
        , Applicative
        , Monad
        , MonadException
        , MonadIO
        , MonadFix
        , MonadFail
        , MonadTrans
        , MonadTransHoist
        , MonadTransTunnel
        )

deriving newtype instance MonadThrow ex m => MonadThrow ex (LifecycleT m)

deriving newtype instance MonadCatch ex m => MonadCatch ex (LifecycleT m)

instance TransConstraint Functor LifecycleT where
    hasTransConstraint = Dict

instance TransConstraint Applicative LifecycleT where
    hasTransConstraint = Dict

instance TransConstraint Monad LifecycleT where
    hasTransConstraint = Dict

instance TransConstraint MonadFail LifecycleT where
    hasTransConstraint = Dict

instance TransConstraint MonadException LifecycleT where
    hasTransConstraint = Dict

instance TransConstraint (MonadThrow e) LifecycleT where
    hasTransConstraint = Dict

instance TransConstraint (MonadCatch e) LifecycleT where
    hasTransConstraint = Dict

instance TransConstraint MonadFix LifecycleT where
    hasTransConstraint = Dict

instance TransConstraint MonadIO LifecycleT where
    hasTransConstraint = Dict

instance MonadTransUnlift LifecycleT where
    liftWithUnlift call = MkLifecycleT $ liftWithUnlift $ \unlift -> call $ unlift . unLifecycleT
    getDiscardingUnlift = MkLifecycleT $ do
        MkWUnlift unlift <- getDiscardingUnlift
        return $ MkWUnlift $ unlift . unLifecycleT

addLifeState :: MonadIO m => LifeState IO -> LifecycleT m ()
addLifeState NoLifeState = return ()
addLifeState ls = MkLifecycleT $ durableWriterTransaction $ tell ls

-- | Add a closing action.
lifecycleOnCloseIO :: MonadIO m => IO () -> LifecycleT m ()
lifecycleOnCloseIO closer = addLifeState $ mkLifeState closer

-- | Add a closing action.
lifecycleOnClose :: MonadAskUnliftIO m => m () -> LifecycleT m ()
lifecycleOnClose closer = do
    MkWRaised unlift <- lift askUnliftIO
    lifecycleOnCloseIO $ unlift closer

-- | Convert a lifecycle to a function that uses the \"with\" pattern.
withLifecycle ::
    forall m a.
    (MonadException m, MonadTunnelIO m) =>
    LifecycleT m a ->
    With m a
withLifecycle (MkLifecycleT rwma) run = runWithExc $ \runExc -> do
    (ma, takeLS) <- liftIO $ runDurableWriterT rwma
    b <- runExc (ma >>= run)
    liftIO $ do
        ls <- takeLS
        closeLifeState ls
    return b

-- | Run the lifecycle, then close all resources in reverse order they were opened.
runLifecycle ::
    forall m.
    (MonadException m, MonadTunnelIO m) =>
    LifecycleT m --> m
runLifecycle lc = withLifecycle lc return

-- | Fork a thread that will complete in this lifecycle. Closing will wait for the thread to finish.
forkLifecycle :: MonadUnliftIO m => m () -> LifecycleT m ThreadId
forkLifecycle action = do
    var <- liftIO newEmptyMVar
    lifecycleOnCloseIO $ takeMVar var
    lift $ liftIOWithUnlift $ \unlift -> forkIO $ finally (unlift action) $ putMVar var ()

-- | Runs a lifecycle, but instead of running the closing actions, return them as a 'LifeState'.
getLifeState ::
    forall m a.
    MonadIO m =>
    LifecycleT m a ->
    m (a, LifeState IO)
getLifeState (MkLifecycleT rwma) = do
    ((a, collector), _) <- runWriterT $ runDurableAsWriterT $ do
        a <- rwma
        collector <- durableWriterGetCollecter
        return (a, collector)
    return (a, execLifeState collector)

{-
runDurableWriterT :: forall w m a. Monoid w => DurableWriterT w m a -> IO (m a, IO w)
runDurableAsWriterT :: forall w m a. (Monoid w, MonadIO m) => DurableWriterT w m a -> WriterT w m a
durableWriterGetCollecter :: forall w m. (Monoid w, MonadIO m) => DurableWriterT w m (IO w)
-}

modifyLifeState ::
    forall m.
    MonadIO m =>
    (LifeState IO -> LifeState IO) ->
    LifecycleT m --> LifecycleT m
modifyLifeState ss la = do
    (a, ls) <- lift $ getLifeState la
    addLifeState $ ss ls
    return a

-- | Runs the given lifecycle, returning a closer.
-- This is how you close things out of order.
--
-- The closer is an idempotent action that will close the lifecycle only if it hasn't already been closed.
-- The closer will also be run as the closer of the resulting lifecycle.
lifecycleGetCloser ::
    forall m a.
    MonadIO m =>
    LifecycleT m a ->
    LifecycleT m (a, IO ())
lifecycleGetCloser lc = do
    (a, ls) <- lift $ getLifeState lc
    var <- liftIO $ newMVar ()
    let
        earlycloser :: IO ()
        earlycloser = do
            mu <- tryTakeMVar var
            case mu of
                Just () -> closeLifeState ls
                Nothing -> return ()
    lifecycleOnCloseIO earlycloser
    return (a, earlycloser)

-- | Returned action returns 'True' if still alive, 'False' if closed.
lifecycleMonitor :: MonadIO m => LifecycleT m (IO Bool)
lifecycleMonitor = do
    ref <- liftIO $ newIORef True
    lifecycleOnCloseIO $ writeIORef ref False
    return $ readIORef ref

-- | Convert a function that uses the \"with\" pattern to a lifecycle.
lifecycleWith :: (MonadCoroutine m, MonadAskUnliftIO m) => With m t -> LifecycleT m t
lifecycleWith withX = do
    (t, closer) <- lift $ unpickWith withX
    lifecycleOnClose closer
    return t

-- | This is the expected most common use.
type Lifecycle = LifecycleT IO
