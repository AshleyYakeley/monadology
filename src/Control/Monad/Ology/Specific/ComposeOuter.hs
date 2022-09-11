module Control.Monad.Ology.Specific.ComposeOuter where

import Control.Monad.Ology.General.Exception.Class
import Control.Monad.Ology.General.Function
import Control.Monad.Ology.General.IO
import Control.Monad.Ology.General.Outer
import Control.Monad.Ology.General.Trans.Constraint
import Control.Monad.Ology.General.Trans.Hoist
import Control.Monad.Ology.General.Trans.Trans
import Control.Monad.Ology.General.Trans.Tunnel
import Import

type ComposeOuter :: (Type -> Type) -> (Type -> Type) -> Type -> Type
newtype ComposeOuter outer inner a = MkComposeOuter
    { unComposeOuter :: outer (inner a)
    }

instance (Foldable inner, Foldable outer, Functor outer) => Foldable (ComposeOuter outer inner) where
    foldMap am (MkComposeOuter oia) = foldMap id $ fmap (foldMap am) oia

instance (Traversable inner, Traversable outer) => Traversable (ComposeOuter outer inner) where
    traverse afb (MkComposeOuter oia) = fmap MkComposeOuter $ traverse (traverse afb) oia

instance Traversable outer => TransConstraint Traversable (ComposeOuter outer) where
    hasTransConstraint = Dict

instance (Functor inner, Functor outer) => Functor (ComposeOuter outer inner) where
    fmap ab (MkComposeOuter oia) = MkComposeOuter $ fmap (fmap ab) oia

instance Functor outer => TransConstraint Functor (ComposeOuter outer) where
    hasTransConstraint = Dict

instance (Applicative inner, Applicative outer) => Applicative (ComposeOuter outer inner) where
    pure a = MkComposeOuter $ pure $ pure a
    MkComposeOuter mab <*> MkComposeOuter ma = MkComposeOuter $ liftA2 (<*>) mab ma

instance Applicative outer => TransConstraint Applicative (ComposeOuter outer) where
    hasTransConstraint = Dict

instance (Monad inner, MonadOuter outer) => Monad (ComposeOuter outer inner) where
    return = pure
    MkComposeOuter oia >>= f =
        MkComposeOuter $ do
            ia <- oia
            MkWExtract oaa <- getExtract
            return $ do
                a <- ia
                oaa $ unComposeOuter $ f a

instance MonadOuter outer => TransConstraint Monad (ComposeOuter outer) where
    hasTransConstraint = Dict

liftOuter :: (Functor outer, Applicative inner) => outer --> ComposeOuter outer inner
liftOuter oa = MkComposeOuter $ fmap pure oa

instance MonadOuter outer => MonadTrans (ComposeOuter outer) where
    lift ma = MkComposeOuter $ pure ma

instance (MonadOuter outer, MonadIO inner) => MonadIO (ComposeOuter outer inner) where
    liftIO ioa = lift $ liftIO ioa

instance MonadOuter outer => TransConstraint MonadIO (ComposeOuter outer) where
    hasTransConstraint = Dict

instance (MonadOuter outer, MonadFail inner) => MonadFail (ComposeOuter outer inner) where
    fail e = MkComposeOuter $ return $ fail e

instance MonadOuter outer => TransConstraint MonadFail (ComposeOuter outer) where
    hasTransConstraint = Dict

instance (MonadOuter outer, MonadFix inner) => MonadFix (ComposeOuter outer inner) where
    mfix f =
        MkComposeOuter $ do
            MkWExtract extract <- getExtract
            return $ mfix $ \a -> extract $ unComposeOuter $ f a

instance MonadOuter outer => TransConstraint MonadFix (ComposeOuter outer) where
    hasTransConstraint = Dict

instance (MonadOuter outer, MonadException m) => MonadException (ComposeOuter outer m) where
    type Exc (ComposeOuter outer m) = Exc m
    throwExc e = lift $ throwExc e
    catchExc tma handler = tunnel $ \unlift -> catchExc (unlift tma) $ \e -> unlift $ handler e

instance MonadOuter outer => MonadTransHoist (ComposeOuter outer) where
    hoist = tunnelHoist

instance MonadOuter outer => MonadTransTunnel (ComposeOuter outer) where
    type Tunnel (ComposeOuter outer) = Identity
    tunnel call =
        MkComposeOuter $ do
            MkWExtract oaa <- getExtract
            return $ fmap runIdentity $ call $ fmap Identity . oaa . unComposeOuter
