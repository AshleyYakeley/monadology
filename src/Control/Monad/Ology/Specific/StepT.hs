module Control.Monad.Ology.Specific.StepT where

import Control.Monad.Ology.General.Function
import Control.Monad.Ology.General.IO
import Control.Monad.Ology.General.Trans.Constraint
import Control.Monad.Ology.General.Trans.Hoist
import Control.Monad.Ology.General.Trans.Trans
import Import

-- | A monad that can be run step-by-step until the result.
newtype StepT f m a = MkStepT
    { unStepT :: m (Either a (f (StepT f m a)))
    }

instance (Functor f, Functor m) => Functor (StepT f m) where
    fmap ab (MkStepT ma) = MkStepT $ fmap (bimap ab $ fmap $ fmap ab) ma

instance Functor f => TransConstraint Functor (StepT f) where
    hasTransConstraint = Dict

instance (Functor f, Monad m) => Applicative (StepT f m) where
    pure a = MkStepT $ pure $ Left a
    mab <*> ma = do
        ab <- mab
        a <- ma
        return $ ab a

instance (Functor f, Monad m) => Monad (StepT f m) where
    return = pure
    MkStepT mea >>= f =
        MkStepT $ do
            ea <- mea
            case ea of
                Left a -> unStepT $ f a
                Right fsa -> return $ Right $ fmap (\sa -> sa >>= f) fsa

instance Functor f => TransConstraint Monad (StepT f) where
    hasTransConstraint = Dict

instance (Functor f, MonadIO m) => MonadIO (StepT f m) where
    liftIO ioa = lift $ liftIO ioa

instance Functor f => TransConstraint MonadIO (StepT f) where
    hasTransConstraint = Dict

instance Functor f => MonadTrans (StepT f) where
    lift ma = MkStepT $ fmap Left ma

instance Functor f => MonadTransHoist (StepT f) where
    hoist f (MkStepT ma) = MkStepT $ (fmap $ fmap $ fmap $ hoist f) $ f ma

-- | Run all the steps until done.
runSteps :: Monad m => Extract f -> StepT f m --> m
runSteps fxx step = do
    eap <- unStepT step
    case eap of
        Left a -> return a
        Right sc -> runSteps fxx $ fxx sc

-- | A pending step for this result.
pendingStep :: (Functor f, Monad m) => f --> StepT f m
pendingStep fa = MkStepT $ pure $ Right $ fmap pure fa
