module Control.Monad.Ology.Specific.Lifecycle.LifecycleT
    ( LifecycleT (..)
    , Lifecycle
    , runLifecycle
    , lifecycleOnCloseIO
    , lifecycleOnClose
    , lifecycleGetCloser
    , forkLifecycle
    , lifecycleMonitor
    , hoistLifecycleClose
    , hoistLifecycleBoth

      -- * With
    , With
    , withLifecycle
    , lifecycleWith

      -- * LifeState
    , LifeState
    , pattern NoLifeState
    , mapLifeState
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
newtype LifecycleT (mc :: Type -> Type) (m :: Type -> Type) (a :: Type) = MkLifecycleT
    { unLifecycleT :: DurableWriterT (LifeState mc) m a
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

deriving newtype instance MonadThrow ex m => MonadThrow ex (LifecycleT mc m)

deriving newtype instance MonadCatch ex m => MonadCatch ex (LifecycleT mc m)

instance TransConstraint Functor (LifecycleT mc) where
    hasTransConstraint = Dict

instance TransConstraint Applicative (LifecycleT mc) where
    hasTransConstraint = Dict

instance TransConstraint Monad (LifecycleT mc) where
    hasTransConstraint = Dict

instance TransConstraint MonadFail (LifecycleT mc) where
    hasTransConstraint = Dict

instance TransConstraint MonadException (LifecycleT mc) where
    hasTransConstraint = Dict

instance TransConstraint (MonadThrow e) (LifecycleT mc) where
    hasTransConstraint = Dict

instance TransConstraint (MonadCatch e) (LifecycleT mc) where
    hasTransConstraint = Dict

instance TransConstraint MonadFix (LifecycleT mc) where
    hasTransConstraint = Dict

instance TransConstraint MonadIO (LifecycleT mc) where
    hasTransConstraint = Dict

instance MonadTransUnlift (LifecycleT mc) where
    liftWithUnlift call = MkLifecycleT $ liftWithUnlift $ \unlift -> call $ unlift . unLifecycleT
    getDiscardingUnlift = MkLifecycleT $ do
        MkWUnlift unlift <- getDiscardingUnlift
        return $ MkWUnlift $ unlift . unLifecycleT

hoistLifecycleClose :: forall mc1 mc2 m. Monad mc1 => (mc1 --> mc2) -> LifecycleT mc1 m --> LifecycleT mc2 m
hoistLifecycleClose f (MkLifecycleT wa) = MkLifecycleT $ mapDurableWriterT (mapLifeState f) wa

hoistLifecycleBoth :: forall m1 m2. (Monad m1, Monad m2) => (m1 --> m2) -> LifecycleT m1 m1 --> LifecycleT m2 m2
hoistLifecycleBoth f = hoist f . hoistLifecycleClose f

addLifeState :: forall mc m. MonadIO m => LifeState mc -> LifecycleT mc m ()
addLifeState NoLifeState = return ()
addLifeState ls = MkLifecycleT $ durableWriterTransaction $ tell ls

-- | Add a closing action.
lifecycleOnClose :: forall mc m. (Monad mc, MonadIO m) => mc () -> LifecycleT mc m ()
lifecycleOnClose closer = addLifeState $ mkLifeState closer

-- | Add a closing action.
lifecycleOnCloseIO :: forall mc m. (MonadIO mc, MonadIO m) => IO () -> LifecycleT mc m ()
lifecycleOnCloseIO closer = lifecycleOnClose $ liftIO closer

-- | Convert a lifecycle to a function that uses the \"with\" pattern.
withLifecycle ::
    forall m a.
    (MonadException m, MonadTunnelIO m) =>
    LifecycleT m m a ->
    With m a
withLifecycle (MkLifecycleT rwma) run = runWithExc $ \runExc -> do
    (ma, takeLS) <- liftIO $ runDurableWriterT rwma
    b <- runExc (ma >>= run)
    ls <- liftIO takeLS
    closeLifeState ls
    return b

-- | Run the lifecycle, then close all resources in reverse order they were opened.
runLifecycle ::
    forall m.
    (MonadException m, MonadTunnelIO m) =>
    LifecycleT m m --> m
runLifecycle lc = withLifecycle lc return

-- | Fork a thread that will complete in this lifecycle. Closing will wait for the thread to finish.
forkLifecycle :: (MonadIO mc, MonadUnliftIO m) => m () -> LifecycleT mc m ThreadId
forkLifecycle action = do
    var <- liftIO newEmptyMVar
    lifecycleOnCloseIO $ takeMVar var
    lift $ liftIOWithUnlift $ \unlift -> forkIO $ finally (unlift action) $ putMVar var ()

-- | Runs a lifecycle, but instead of running the closing actions, return them as a 'LifeState'.
getLifeState ::
    forall mc m a.
    (MonadIO mc, MonadIO m) =>
    LifecycleT mc m a ->
    m (a, LifeState mc)
getLifeState (MkLifecycleT rwma) = do
    (ma, collector) <- liftIO $ runDurableWriterT rwma
    a <- ma
    return (a, execLifeState $ liftIO collector)

modifyLifeState ::
    forall mc m.
    (MonadIO mc, MonadIO m) =>
    (LifeState mc -> LifeState mc) ->
    LifecycleT mc m --> LifecycleT mc m
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
    forall mc m a.
    (MonadIO mc, MonadIO m) =>
    LifecycleT mc m a ->
    LifecycleT mc m (a, mc ())
lifecycleGetCloser lc = do
    (a, ls) <- lift $ getLifeState lc
    var <- liftIO $ newMVar ()
    let
        earlycloser :: mc ()
        earlycloser = do
            mu <- liftIO $ tryTakeMVar var
            case mu of
                Just () -> closeLifeState ls
                Nothing -> return ()
    lifecycleOnClose earlycloser
    return (a, earlycloser)

-- | Returned action returns 'True' if still alive, 'False' if closed.
lifecycleMonitor :: (MonadIO mc, MonadIO m) => LifecycleT mc m (IO Bool)
lifecycleMonitor = do
    ref <- liftIO $ newIORef True
    lifecycleOnCloseIO $ writeIORef ref False
    return $ readIORef ref

-- | Convert a function that uses the \"with\" pattern to a lifecycle.
lifecycleWith :: (MonadCoroutine m, MonadIO m) => With m t -> LifecycleT m m t
lifecycleWith withX = do
    (t, closer) <- lift $ unpickWith withX
    lifecycleOnClose closer
    return t

-- | This is the expected most common use.
type Lifecycle = LifecycleT IO IO
