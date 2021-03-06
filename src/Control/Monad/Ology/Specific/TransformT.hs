module Control.Monad.Ology.Specific.TransformT where

import Control.Monad.Ology.Data
import Control.Monad.Ology.General
import Import

type TransformT :: forall k. (k -> Type) -> Type -> Type
newtype TransformT f a = MkTransformT
    { runTransformT :: forall r. (a -> f r) -> f r
    }

instance Functor (TransformT f) where
    fmap ab (MkTransformT aff) = MkTransformT $ \bf -> aff $ bf . ab

instance TransConstraint Functor TransformT where
    hasTransConstraint = Dict

instance Applicative (TransformT f) where
    pure a = MkTransformT $ \af -> af a
    MkTransformT f <*> MkTransformT x = MkTransformT $ \bf -> f $ \ab -> x (bf . ab)

instance TransConstraint Applicative TransformT where
    hasTransConstraint = Dict

instance Monad (TransformT f) where
    return = pure
    MkTransformT m >>= f = MkTransformT $ \bf -> m (\a -> runTransformT (f a) bf)

instance TransConstraint Monad TransformT where
    hasTransConstraint = Dict

instance MonadTrans TransformT where
    lift m = MkTransformT $ \af -> m >>= af

instance MonadIO m => MonadIO (TransformT m) where
    liftIO = lift . liftIO

instance TransConstraint MonadIO TransformT where
    hasTransConstraint = Dict

instance (Functor f, Semigroup a) => Semigroup (TransformT f a) where
    (<>) = liftA2 (<>)

instance (Functor f, Monoid a) => Monoid (TransformT f a) where
    mempty = pure mempty

mapTransformT :: (f --> f) -> TransformT f ()
mapTransformT ff = MkTransformT $ \uf -> ff $ uf ()

execMapTransformT :: Monad f => f (TransformT f a) -> TransformT f a
execMapTransformT ffa =
    MkTransformT $ \af -> do
        MkTransformT aff <- ffa
        aff af

transformParamRef ::
       forall m a. Monad m
    => Param m a
    -> Ref (TransformT m) a
transformParamRef param = let
    refGet :: TransformT m a
    refGet =
        MkTransformT $ \afr -> do
            a <- paramAsk param
            afr a
    refPut :: a -> TransformT m ()
    refPut a = MkTransformT $ \ufr -> paramWith param a $ ufr ()
    in MkRef {..}

liftTransformT ::
       forall t m. (MonadTransUnlift t, MonadTunnelIOInner m)
    => TransformT m --> TransformT (t m)
liftTransformT (MkTransformT aff) = MkTransformT $ \atf -> liftWithUnlift $ \unlift -> aff $ unlift . atf
