module Control.Monad.Ology.Specific.StepT where

import Control.Monad.Ology.General.IO
import Control.Monad.Ology.General.Trans.Constraint
import Control.Monad.Ology.General.Trans.Hoist
import Control.Monad.Ology.General.Trans.Trans
import Import

newtype StepT f m a = MkStepT
    { singleStep :: m (Either a (f (StepT f m a)))
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
                Left a -> singleStep $ f a
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

runSteps :: Monad m => (forall x. f x -> x) -> StepT f m a -> m a
runSteps fxx step = do
    eap <- singleStep step
    case eap of
        Left a -> return a
        Right sc -> runSteps fxx $ fxx sc

pendingStep :: (Functor f, Monad m) => f a -> StepT f m a
pendingStep fa = MkStepT $ pure $ Right $ fmap pure fa
