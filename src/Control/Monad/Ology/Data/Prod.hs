module Control.Monad.Ology.Data.Prod where

import Control.Monad.Ology.General
import Control.Monad.Ology.Specific.WriterT
import Import

-- | A product of a monad (as in 'WriterT').
data Prod m a = MkProd
    { prodTell :: a -> m ()
    , prodListen :: forall r. m r -> m (r, a)
    }

instance Functor m => Invariant (Prod m) where
    invmap f g (MkProd t l) = MkProd (t . g) (\mr -> fmap (fmap f) $ l mr)

instance Applicative m => Productable (Prod m) where
    pUnit = MkProd (\() -> pure ()) $ fmap $ \r -> (r, ())
    (<***>) :: forall a b. Prod m a -> Prod m b -> Prod m (a, b)
    MkProd tellA listenA <***> MkProd tellB listenB = let
        tellAB :: (a, b) -> m ()
        tellAB (a, b) = tellA a *> tellB b
        listenAB :: m r -> m (r, (a, b))
        listenAB m = fmap (\((r, a), b) -> (r, (a, b))) $ listenB (listenA m)
        in MkProd tellAB listenAB

prodListen_ :: Functor m => Prod m a -> m () -> m a
prodListen_ p mu = fmap snd $ prodListen p mu

liftProd :: (MonadTransTunnel t, Monad m) => Prod m --> Prod (t m)
liftProd (MkProd t l) =
    MkProd (\a -> lift $ t a) $ \tmr -> tunnel $ \unlift -> fmap (\(tun, a) -> fmap (\r -> (r, a)) tun) $ l $ unlift tmr

writerProd :: Monad m => Prod (WriterT w m) w
writerProd = MkProd {prodTell = tell, prodListen = listen}

foldProd ::
       forall f m a. (Applicative f, Foldable f, Applicative m)
    => Prod m a
    -> Prod m (f a)
foldProd (MkProd prodTell prodListen) = let
    prodTell' :: f a -> m ()
    prodTell' aa = for_ aa prodTell
    prodListen' :: forall r. m r -> m (r, f a)
    prodListen' mr = fmap (\(r, a) -> (r, pure a)) $ prodListen mr
    in MkProd prodTell' prodListen'
