module Control.Monad.Ology.Specific.Durable
    ( DurableStateT (..)
    , runDurableStateT
    , runDurableAsStateT
    , durableStateTransaction
    , nondurableStateTransaction
    , DurableWriterT
    , runDurableWriterT
    , runDurableAsWriterT
    , durableWriterTransaction
    , subDurableWriterT
    , durableWriterGetCollecter
    )
where

import Control.Monad.Ology.General
import Control.Monad.Ology.Specific.ReaderT
import Control.Monad.Ology.Specific.StateT
import Control.Monad.Ology.Specific.WriterT
import Import

-- | A state monad that persists state through exceptions
newtype DurableStateT (s :: Type) (m :: Type -> Type) (a :: Type) = MkDurableStateT
    { unDurableStateT :: ReaderT (MVar s) m a
    }
    deriving newtype
        ( Functor
        , Applicative
        , Alternative
        , Monad
        , MonadException
        , MonadIO
        , MonadFail
        , MonadFix
        , MonadPlus
        , MonadTrans
        , MonadTransHoist
        , MonadTransTunnel
        , MonadThrow ex
        , MonadCatch ex
        )

instance TransConstraint Functor (DurableStateT s) where
    hasTransConstraint = Dict

instance TransConstraint Monad (DurableStateT s) where
    hasTransConstraint = Dict

instance TransConstraint MonadIO (DurableStateT s) where
    hasTransConstraint = Dict

instance TransConstraint MonadFail (DurableStateT s) where
    hasTransConstraint = Dict

instance TransConstraint MonadFix (DurableStateT s) where
    hasTransConstraint = Dict

instance TransConstraint MonadPlus (DurableStateT s) where
    hasTransConstraint = Dict

instance MonadTransUnlift (DurableStateT s) where
    liftWithUnlift call = MkDurableStateT $ liftWithUnlift $ \unlift -> call $ unlift . unDurableStateT
    getDiscardingUnlift = MkDurableStateT $ do
        oldVar <- ask
        olds <- liftIO $ readMVar oldVar
        return
            $ MkWUnlift
            $ \(MkDurableStateT rvma) -> do
                newVar <- liftIO $ newMVar olds
                runReaderT rvma newVar

runDurableStateT :: forall s m a. DurableStateT s m a -> s -> IO (m a, IO s)
runDurableStateT (MkDurableStateT (ReaderT vma)) olds = do
    var <- newMVar olds
    return (vma var, takeMVar var)

runDurableAsStateT :: forall s m a. MonadIO m => DurableStateT s m a -> StateT s m a
runDurableAsStateT dma = StateT $ \olds -> do
    (run, takeState) <- liftIO $ runDurableStateT dma olds
    a <- run
    news <- liftIO takeState
    return (a, news)

durableStateTransaction :: forall s m a. MonadTunnelIO m => StateT s m a -> DurableStateT s m a
durableStateTransaction sma = MkDurableStateT $ ReaderT $ \var -> mVarRunStateT var sma

nondurableStateTransaction :: forall s m a. MonadIO m => StateT s m a -> DurableStateT s m a
nondurableStateTransaction sma = MkDurableStateT $ ReaderT $ \var -> dangerousMVarRunStateT var sma

type DurableWriterT w = DurableStateT w

stateWriter :: forall w m a. Monoid w => StateT w m a -> WriterT w m a
stateWriter (StateT wma) = WriterT $ wma mempty

runDurableWriterT :: forall w m a. Monoid w => DurableWriterT w m a -> IO (m a, IO w)
runDurableWriterT dma = runDurableStateT dma mempty

runDurableAsWriterT :: forall w m a. (Monoid w, MonadIO m) => DurableWriterT w m a -> WriterT w m a
runDurableAsWriterT dwa = stateWriter $ runDurableAsStateT dwa

subDurableWriterT :: forall w1 w2 m a. (Monoid w1, Monoid w2, MonadIO m) => (w1 -> w2) -> DurableWriterT w1 m a -> DurableWriterT w2 m a
subDurableWriterT f = durableWriterTransaction . mapWriterOutput f . runDurableAsWriterT

durableWriterTransaction :: forall w m a. (Monoid w, MonadIO m) => WriterT w m a -> DurableWriterT w m a
durableWriterTransaction wma = do
    (a, w) <- lift $ runWriterT wma
    nondurableStateTransaction $ do
        olds <- get
        put $ olds <> w
        return a

durableWriterGetCollecter :: forall w m. (Monoid w, MonadIO m) => DurableWriterT w m (IO w)
durableWriterGetCollecter =
    MkDurableStateT
        $ ReaderT
        $ \var ->
            return
                $ mVarRunStateT var
                $ do
                    w <- get
                    put mempty
                    return w
