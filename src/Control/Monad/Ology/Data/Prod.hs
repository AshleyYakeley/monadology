module Control.Monad.Ology.Data.Prod where

import Control.Monad.Ology.General
import Control.Monad.Ology.Specific.WriterT
import Import

-- | A product of a monad (as in 'WriterT').
data Prod m a = MkProd
    { prodTell :: a -> m ()
    , prodCollect :: forall r. m r -> m (r, a)
    }

instance Functor m => Invariant (Prod m) where
    invmap f g (MkProd t l) = MkProd (t . g) (\mr -> fmap (fmap f) $ l mr)

instance Applicative m => Productable (Prod m) where
    rUnit = MkProd (\() -> pure ()) $ fmap $ \r -> (r, ())
    (<***>) :: forall a b. Prod m a -> Prod m b -> Prod m (a, b)
    MkProd tellA collectA <***> MkProd tellB collectB = let
        tellAB :: (a, b) -> m ()
        tellAB (a, b) = tellA a *> tellB b
        collectAB :: m r -> m (r, (a, b))
        collectAB m = fmap (\((r, a), b) -> (r, (a, b))) $ collectB (collectA m)
        in MkProd tellAB collectAB

prodCollect_ :: Functor m => Prod m a -> m () -> m a
prodCollect_ p mu = fmap snd $ prodCollect p mu

prodListen :: Monad m => Prod m a -> forall r. m r -> m (r, a)
prodListen p mr = do
    (r, a) <- prodCollect p mr
    prodTell p a
    return (r, a)

prodListen_ :: Monad m => Prod m a -> m () -> m a
prodListen_ p mu = fmap snd $ prodListen p mu

prodPass :: Monad m => Prod m a -> m (r, a -> a) -> m r
prodPass p mraa = do
    ((r, f), a) <- prodCollect p mraa
    prodTell p $ f a
    return r

prodCensor :: Monad m => Prod m a -> (a -> a) -> m --> m
prodCensor p f mr = do
    (r, a) <- prodCollect p mr
    prodTell p $ f a
    return r

prodTellItem :: Applicative f => Prod m (f a) -> a -> m ()
prodTellItem p a = prodTell p $ pure a

prodCensorItems :: (Monad f, Monad m) => Prod m (f a) -> (a -> f a) -> m --> m
prodCensorItems p afa = prodCensor p $ \fa -> fa >>= afa

liftProd :: (MonadTransTunnel t, Monad m) => Prod m --> Prod (t m)
liftProd (MkProd t l) =
    MkProd (\a -> lift $ t a) $ \tmr -> tunnel $ \unlift -> fmap (\(tun, a) -> fmap (\r -> (r, a)) tun) $ l $ unlift tmr

writerProd :: (Monad m, Monoid w) => Prod (WriterT w m) w
writerProd = MkProd {prodTell = tell, prodCollect = collect}

foldProd ::
       forall f m a. (Applicative f, Foldable f, Applicative m)
    => Prod m a
    -> Prod m (f a)
foldProd (MkProd prodTell prodCollect) = let
    prodTell' :: f a -> m ()
    prodTell' aa = for_ aa prodTell
    prodCollect' :: forall r. m r -> m (r, f a)
    prodCollect' mr = fmap (\(r, a) -> (r, pure a)) $ prodCollect mr
    in MkProd prodTell' prodCollect'
