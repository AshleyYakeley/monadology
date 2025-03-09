module Control.Monad.Ology.Specific.WithT where

import Control.Monad.Ology.Data
import Control.Monad.Ology.General
import Import

type WithT :: forall k. (k -> Type) -> Type -> Type
newtype WithT m a = MkWithT
    { unWithT :: With m a
    }

instance Functor (WithT m) where
    fmap ab (MkWithT aff) = MkWithT $ \bf -> aff $ bf . ab

instance TransConstraint Functor WithT where
    hasTransConstraint = Dict

instance Applicative (WithT m) where
    pure a = MkWithT $ \af -> af a
    MkWithT f <*> MkWithT x = MkWithT $ \bf -> f $ \ab -> x (bf . ab)

instance TransConstraint Applicative WithT where
    hasTransConstraint = Dict

instance Monad (WithT m) where
    return = pure
    MkWithT m >>= f = MkWithT $ \bf -> m (\a -> unWithT (f a) bf)

instance TransConstraint Monad WithT where
    hasTransConstraint = Dict

instance MonadTrans WithT where
    lift m = MkWithT $ \af -> m >>= af

instance MonadIO m => MonadIO (WithT m) where
    liftIO = lift . liftIO

instance TransConstraint MonadIO WithT where
    hasTransConstraint = Dict

instance Semigroup a => Semigroup (WithT m a) where
    (<>) = liftA2 (<>)

instance Monoid a => Monoid (WithT m a) where
    mempty = pure mempty

instance MonadFix m => MonadFix (WithT m) where
    mfix ama =
        MkWithT $ \amr -> do
            rec (~(olda, r')) <-
                    unWithT (ama olda) $ \newa -> do
                        r <- amr newa
                        return (newa, r)
            return r'

instance MonadException m => MonadException (WithT m) where
    type Exc (WithT m) = Exc m
    throwExc e = MkWithT $ \_ -> throwExc e
    catchExc (MkWithT afrfr) cc = MkWithT $ \afr -> catchExc (afrfr afr) $ \e -> unWithT (cc e) afr

instance MonadThrow e m => MonadThrow e (WithT m) where
    throw e = MkWithT $ \_ -> throw e

instance MonadCatch e m => MonadCatch e (WithT m) where
    catch (MkWithT afrfr) cc = MkWithT $ \afr -> catch (afrfr afr) $ \e -> unWithT (cc e) afr

unpickWithT ::
    forall m a.
    MonadCoroutine m =>
    WithT m a ->
    m (a, m ())
unpickWithT (MkWithT w) = unpickWith w

pickWithT ::
    forall m a.
    Monad m =>
    m (a, m ()) ->
    WithT m a
pickWithT mm = MkWithT $ pickWith mm

instance {-# OVERLAPPING #-} (MonadHoistIO m, MonadCoroutine m) => MonadHoistIO (WithT m) where
    hoistIO f wma = pickWithT $ fmap (fmap $ hoistIO f) $ hoistIO f $ unpickWithT wma

mapWithT :: (m --> m) -> WithT m ()
mapWithT ff = MkWithT $ \uf -> ff $ uf ()

postWithT :: Monad m => m () -> WithT m ()
postWithT mu =
    mapWithT $ \mr -> do
        r <- mr
        mu
        return r

withTMap :: WithT m () -> m --> m
withTMap (MkWithT uff) f = uff $ \() -> f

execMapWithT :: Monad m => m (WithT m a) -> WithT m a
execMapWithT ffa =
    MkWithT $ \af -> do
        MkWithT aff <- ffa
        aff af

withParamRef ::
    forall m.
    Monad m =>
    Param m --> Ref (WithT m)
withParamRef (param :: _ a) = let
    refGet :: WithT m a
    refGet =
        MkWithT $ \afr -> do
            a <- paramAsk param
            afr a
    refPut :: a -> WithT m ()
    refPut a = MkWithT $ \ufr -> paramWith param a $ ufr ()
    in MkRef{..}

liftWithT ::
    forall t m.
    (MonadTransUnlift t, MonadTunnelIO m) =>
    WithT m --> WithT (t m)
liftWithT (MkWithT aff) = MkWithT $ \atf -> liftWithUnlift $ \unlift -> aff $ unlift . atf
