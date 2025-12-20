module Control.Monad.Ology.Specific.ComposeCommute where

import Control.Monad.Ology.General.Commute
import Control.Monad.Ology.General.Trans.Constraint
import Import

type ComposeCommute :: (Type -> Type) -> (Type -> Type) -> Type -> Type
newtype ComposeCommute outer inner a = MkComposeCommute
    { unComposeCommute :: outer (inner a)
    }

instance forall outer inner. (Foldable inner, Foldable outer, Functor outer) => Foldable (ComposeCommute outer inner) where
    foldMap am (MkComposeCommute oia) = foldMap id $ fmap (foldMap am) oia

instance forall outer inner. (Traversable inner, Traversable outer) => Traversable (ComposeCommute outer inner) where
    traverse afb (MkComposeCommute oia) = fmap MkComposeCommute $ traverse (traverse afb) oia

instance forall outer. Traversable outer => TransConstraint Traversable (ComposeCommute outer) where
    hasTransConstraint = Dict

instance forall outer inner. (Functor inner, Functor outer) => Functor (ComposeCommute outer inner) where
    fmap ab (MkComposeCommute oia) = MkComposeCommute $ fmap (fmap ab) oia

instance forall outer. Functor outer => TransConstraint Functor (ComposeCommute outer) where
    hasTransConstraint = Dict

instance forall outer inner. (Applicative inner, Applicative outer) => Applicative (ComposeCommute outer inner) where
    pure a = MkComposeCommute $ pure $ pure a
    MkComposeCommute mab <*> MkComposeCommute ma = MkComposeCommute $ liftA2 (<*>) mab ma

instance forall outer. Applicative outer => TransConstraint Applicative (ComposeCommute outer) where
    hasTransConstraint = Dict

instance forall outer inner. (MonadCommute outer, Monad inner, Traversable inner) => Monad (ComposeCommute outer inner) where
    return = pure
    MkComposeCommute oia >>= f =
        MkComposeCommute $ do
            ia <- oia
            iib <- for ia $ unComposeCommute . f
            pure $ join iib

instance forall outer inner. (MonadCommute outer, MonadCommute inner, Traversable inner) => MonadCommute (ComposeCommute outer inner)
