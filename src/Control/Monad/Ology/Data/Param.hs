module Control.Monad.Ology.Data.Param where

import Control.Monad.Ology.General
import Control.Monad.Ology.Specific.ReaderT
import Import

-- | borrowed from the lens package
type Lens' a b = forall f. Functor f => (b -> f b) -> a -> f a

data Param m a = MkParam
    { paramAsk :: m a
    , paramWith :: a -> m --> m
    }

instance Functor m => Invariant (Param m) where
    invmap f g (MkParam a w) = MkParam (fmap f a) (\b mr -> w (g b) mr)

instance Applicative m => Productish (Param m) where
    pUnit = MkParam (pure ()) (\() -> id)
    pa <***> pb = MkParam (liftA2 (,) (paramAsk pa) (paramAsk pb)) (\(a, b) -> paramWith pa a . paramWith pb b)

paramLocal ::
       forall m a. Monad m
    => Param m a
    -> (a -> a)
    -> m --> m
paramLocal param f mr = do
    a <- paramAsk param
    paramWith param (f a) mr

lensMapParam ::
       forall m a b. Monad m
    => Lens' a b
    -> Param m a
    -> Param m b
lensMapParam l param = let
    paramAsk' = fmap (\a -> getConst $ l Const a) $ paramAsk param
    paramWith' :: b -> m --> m
    paramWith' b mr = do
        a <- paramAsk param
        paramWith param (runIdentity $ l (\_ -> Identity b) a) mr
    in MkParam paramAsk' paramWith'

liftParam :: (MonadTransTunnel t, Monad m) => Param m --> Param (t m)
liftParam (MkParam a l) = MkParam (lift a) $ \aa -> hoist $ l aa

readerParam ::
       forall m r. Monad m
    => Param (ReaderT r m) r
readerParam = MkParam ask $ \r -> with r
