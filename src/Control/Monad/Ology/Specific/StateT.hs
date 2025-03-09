{-# OPTIONS -fno-warn-orphans #-}

module Control.Monad.Ology.Specific.StateT
    ( module Control.Monad.Trans.State
    , module Control.Monad.Ology.Specific.StateT
    )
where

import Control.Monad.Trans.State hiding (liftCallCC, liftCatch, liftListen, liftPass)

import Control.Monad.Ology.General
import Import

instance TransConstraint Functor (StateT s) where
    hasTransConstraint = Dict

instance TransConstraint Monad (StateT s) where
    hasTransConstraint = Dict

instance TransConstraint MonadIO (StateT s) where
    hasTransConstraint = Dict

instance TransConstraint MonadFail (StateT s) where
    hasTransConstraint = Dict

instance TransConstraint MonadFix (StateT s) where
    hasTransConstraint = Dict

instance TransConstraint MonadPlus (StateT s) where
    hasTransConstraint = Dict

instance MonadTransCoerce (StateT a) where
    transCoerce = Dict

instance MonadException m => MonadException (StateT s m) where
    type Exc (StateT s m) = Exc m
    throwExc e = lift $ throwExc e
    catchExc tma handler = tunnel $ \unlift -> catchExc (unlift tma) $ \e -> unlift $ handler e

instance TransConstraint MonadException (StateT s) where
    hasTransConstraint = Dict

instance MonadThrow e m => MonadThrow e (StateT s m) where
    throw e = lift $ throw e

instance TransConstraint (MonadThrow e) (StateT s) where
    hasTransConstraint = Dict

instance MonadCatch e m => MonadCatch e (StateT s m) where
    catch ma handler = tunnel $ \unlift -> catch (unlift ma) $ \e -> unlift $ handler e

instance TransConstraint (MonadCatch e) (StateT s) where
    hasTransConstraint = Dict

instance MonadTransHoist (StateT s) where
    hoist = tunnelHoist

instance MonadTransTunnel (StateT s) where
    type Tunnel (StateT s) = (,) (Endo s)
    tunnel call =
        StateT $ \olds ->
            fmap (\(Endo sf, r) -> (r, sf olds))
                $ call
                $ \(StateT smrs) -> fmap (\(a, s) -> (Endo $ pure s, a)) $ smrs olds

instance MonadTransUnlift (StateT s) where
    liftWithUnlift call = liftWithMVarStateT $ \var -> call $ mVarRunStateT var

-- | Run the 'StateT' on an 'MVar', taking the initial state and putting the final state.
mVarRunStateT :: MVar s -> Unlift MonadTunnelIO (StateT s)
mVarRunStateT var (StateT smr) =
    tunnelIO $ \unlift ->
        modifyMVar var $ \olds ->
            fmap (\fas -> (fromMaybe olds $ mToMaybe $ fmap snd fas, fmap fst fas)) $ unlift $ smr olds

-- | Take the 'MVar' before and put it back after.
mVarRunLocked :: MonadTunnelIO m => MVar s -> m --> m
mVarRunLocked var ma = mVarRunStateT var $ lift ma

discardingStateTUnlift :: s -> Unlift MonadIO (StateT s)
discardingStateTUnlift s mr = do
    (r, _discarded) <- runStateT mr s
    return r

-- | Dangerous, because the MVar won't be released on exception.
dangerousMVarRunStateT :: MVar s -> Unlift MonadIO (StateT s)
dangerousMVarRunStateT var (StateT smr) = do
    olds <- liftIO $ takeMVar var
    (a, news) <- smr olds
    liftIO $ putMVar var news
    return a

liftStateT :: (Traversable f, Applicative m) => StateT s m a -> StateT (f s) m (f a)
liftStateT (StateT smas) = StateT $ \fs -> fmap (\fas -> (fmap fst fas, fmap snd fas)) $ traverse smas fs

liftWithMVarStateT :: MonadIO m => (MVar s -> m a) -> StateT s m a
liftWithMVarStateT vma =
    StateT $ \initialstate -> do
        var <- liftIO $ newMVar initialstate
        r <- vma var
        finalstate <- liftIO $ takeMVar var
        return (r, finalstate)
