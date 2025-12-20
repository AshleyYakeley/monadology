{-# OPTIONS -Wno-redundant-constraints #-}
module Control.Monad.Ology.General.Commute where

import Control.Monad.Ology.General.Trans.Tunnel
import Import

class Monad m => MonadCommute m

instance MonadCommute Identity

instance MonadCommute Maybe

instance MonadCommute ((->) r)

instance (Monoid w, Commutative w) => MonadCommute ((,) w)

instance {-# OVERLAPPABLE #-} (MonadTransTunnel t, MonadCommute (Tunnel t), MonadCommute m) => MonadCommute (t m)
