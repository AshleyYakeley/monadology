module Control.Monad.Ology.Specific.LifecycleT
    ( LifecycleT(..)
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
    ) where

import Control.Monad.Ology.General
import Control.Monad.Ology.Specific.StateT
import Import

-- | This represents all the actions that need to be done when closing the lifecycle.
newtype LifeState =
    MkLifeState (Maybe (IO (IO Any)))

pattern NoLifeState :: LifeState

pattern NoLifeState = MkLifeState Nothing

lifeStateModify :: (IO --> IO) -> LifeState -> LifeState
lifeStateModify _ (MkLifeState Nothing) = MkLifeState Nothing
lifeStateModify m (MkLifeState (Just ioioa)) = MkLifeState $ Just $ m $ fmap m ioioa

closeIOAny :: IO Any -> IO ()
closeIOAny ioa = do
    Any b <- ioa
    if b
        then closeIOAny ioa
        else return ()

closeLifeState' :: LifeState -> IO Any
closeLifeState' (MkLifeState (Just ioioa)) = do
    ioa <- ioioa
    closeIOAny ioa
    return $ Any True
closeLifeState' (MkLifeState Nothing) = return $ Any False

closeLifeState :: LifeState -> IO ()
closeLifeState ls = do
    _ <- closeLifeState' ls
    return ()

varLifeState :: MVar LifeState -> LifeState
varLifeState var =
    MkLifeState $
    Just $
    return $ do
        ls <- takeMVar var
        putMVar var mempty
        closeLifeState' ls

instance Semigroup LifeState where
    MkLifeState Nothing <> q = q
    p <> MkLifeState Nothing = p
    MkLifeState (Just p) <> MkLifeState (Just q) = MkLifeState $ Just $ p <> q

instance Monoid LifeState where
    mempty = MkLifeState Nothing

-- | This is for managing the automatic closing of opened resources.
newtype LifecycleT m a = MkLifecycleT
    { unLifecycleT :: MVar LifeState -> m a
    }

instance Functor m => Functor (LifecycleT m) where
    fmap ab (MkLifecycleT f) = MkLifecycleT $ \var -> fmap ab $ f var

instance TransConstraint Functor LifecycleT where
    hasTransConstraint = Dict

instance Applicative m => Applicative (LifecycleT m) where
    pure t = MkLifecycleT $ \_ -> pure t
    (MkLifecycleT ocab) <*> (MkLifecycleT oca) = MkLifecycleT $ \var -> ocab var <*> oca var

instance TransConstraint Applicative LifecycleT where
    hasTransConstraint = Dict

instance Monad m => Monad (LifecycleT m) where
    return = pure
    (MkLifecycleT va) >>= f =
        MkLifecycleT $ \var -> do
            a <- va var
            unLifecycleT (f a) var

instance TransConstraint Monad LifecycleT where
    hasTransConstraint = Dict

instance MonadTrans LifecycleT where
    lift ma = MkLifecycleT $ \_ -> ma

instance MonadFail m => MonadFail (LifecycleT m) where
    fail s = lift $ fail s

instance TransConstraint MonadFail LifecycleT where
    hasTransConstraint = Dict

instance MonadException m => MonadException (LifecycleT m) where
    type Exc (LifecycleT m) = Exc m
    throwExc e = lift $ throwExc e
    catchExc :: forall a. LifecycleT m a -> (Exc m -> LifecycleT m a) -> LifecycleT m a
    catchExc (MkLifecycleT f) handler = MkLifecycleT $ \var -> catchExc (f var) $ \e -> unLifecycleT (handler e) var

instance TransConstraint MonadException LifecycleT where
    hasTransConstraint = Dict

instance MonadThrow e m => MonadThrow e (LifecycleT m) where
    throw e = lift $ throw e

instance TransConstraint (MonadThrow e) LifecycleT where
    hasTransConstraint = Dict

instance MonadCatch e m => MonadCatch e (LifecycleT m) where
    catch (MkLifecycleT f) handler = MkLifecycleT $ \var -> catch (f var) $ \e -> unLifecycleT (handler e) var

instance TransConstraint (MonadCatch e) LifecycleT where
    hasTransConstraint = Dict

instance MonadFix m => MonadFix (LifecycleT m) where
    mfix f = MkLifecycleT $ \var -> mfix $ \a -> unLifecycleT (f a) var

instance TransConstraint MonadFix LifecycleT where
    hasTransConstraint = Dict

instance MonadIO m => MonadIO (LifecycleT m) where
    liftIO ioa = lift $ liftIO ioa

instance TransConstraint MonadIO LifecycleT where
    hasTransConstraint = Dict

instance MonadTransHoist LifecycleT where
    hoist f (MkLifecycleT g) = MkLifecycleT $ \var -> f $ g var

instance MonadTransTunnel LifecycleT where
    type Tunnel LifecycleT = Identity
    tunnel ::
           forall m r. Monad m
        => ((forall m1 a. Monad m1 => LifecycleT m1 a -> m1 (Identity a)) -> m (Identity r))
        -> LifecycleT m r
    tunnel f = MkLifecycleT $ \var -> fmap runIdentity $ f $ \a -> fmap Identity $ unLifecycleT a var

instance MonadTransUnlift LifecycleT where
    liftWithUnlift call = MkLifecycleT $ \var -> call $ \(MkLifecycleT f) -> f var
    getDiscardingUnlift =
        return $
        MkWUnlift $ \(MkLifecycleT f) -> do
            var <- liftIO $ newMVar mempty
            f var

addLifeState :: MonadIO m => LifeState -> LifecycleT m ()
addLifeState (MkLifeState Nothing) = return ()
addLifeState ls =
    MkLifecycleT $ \var -> do
        dangerousMVarRunStateT var $ do
            s <- get
            put $ ls <> s

-- | Add a closing action.
lifecycleOnCloseIO :: MonadIO m => IO () -> LifecycleT m ()
lifecycleOnCloseIO closer =
    addLifeState $
    MkLifeState $
    Just $ do
        closer
        return $ return $ Any False

-- | Add a closing action.
lifecycleOnClose :: MonadAskUnliftIO m => m () -> LifecycleT m ()
lifecycleOnClose closer = do
    MkWRaised unlift <- lift askUnliftIO
    lifecycleOnCloseIO $ unlift closer

-- | Convert a lifecycle to a function that uses the \"with\" pattern.
withLifecycle ::
       forall m a. (MonadException m, MonadTunnelIO m)
    => LifecycleT m a
    -> With m a
withLifecycle (MkLifecycleT f) run = do
    var <- liftIO $ newMVar mempty
    finally (f var >>= run) $ liftIO $ closeLifeState $ varLifeState var

-- | Run the lifecycle, then close all resources in reverse order they were opened.
runLifecycle ::
       forall m. (MonadException m, MonadTunnelIO m)
    => LifecycleT m --> m
runLifecycle lc = withLifecycle lc return

-- | Fork a thread that will complete in this lifecycle. Closing will wait for the thread to finish.
forkLifecycle :: MonadUnliftIO m => m () -> LifecycleT m ThreadId
forkLifecycle action = do
    var <- liftIO newEmptyMVar
    lifecycleOnCloseIO $ takeMVar var
    lift $ liftIOWithUnlift $ \unlift -> forkIO $ finally (unlift action) $ putMVar var ()

-- | Runs a lifecycle, but instead of running the closing actions, return them as a 'LifeState'.
getLifeState ::
       forall m a. MonadIO m
    => LifecycleT m a
    -> m (a, LifeState)
getLifeState (MkLifecycleT f) = do
    var <- liftIO $ newMVar mempty
    t <- f var
    return (t, varLifeState var)

modifyLifeState ::
       forall m. MonadIO m
    => (LifeState -> LifeState)
    -> LifecycleT m --> LifecycleT m
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
       forall m a. MonadIO m
    => LifecycleT m a
    -> LifecycleT m (a, IO ())
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
