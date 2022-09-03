module Control.Monad.Ology.Data.Ref where

import Control.Monad.Ology.Data.Param
import Control.Monad.Ology.Data.Prod
import Control.Monad.Ology.General
import Control.Monad.Ology.Specific.StateT
import qualified Control.Monad.ST.Lazy as Lazy
import qualified Control.Monad.ST.Strict as Strict
import Data.IORef
import qualified Data.STRef.Lazy as Lazy
import qualified Data.STRef.Strict as Strict
import Import

data Ref m a = MkRef
    { refGet :: m a
    , refPut :: a -> m ()
    }

instance Functor m => Invariant (Ref m) where
    invmap f g (MkRef gt pt) = MkRef (fmap f gt) (pt . g)

instance Applicative m => Productable (Ref m) where
    pUnit = MkRef (pure ()) (\_ -> pure ())
    ra <***> rb = MkRef (liftA2 (,) (refGet ra) (refGet rb)) $ \(a, b) -> refPut ra a *> refPut rb b

refModify :: Monad m => Ref m a -> (a -> a) -> m ()
refModify ref f = do
    a <- refGet ref
    refPut ref $ f a

refModifyM :: Monad m => Ref m a -> (a -> m a) -> m ()
refModifyM ref f = do
    a <- refGet ref
    a' <- f a
    refPut ref a'

refRestore :: (MonadUnliftIO m, MonadException m) => Ref m a -> m --> m
refRestore ref mr = bracket (refGet ref) (refPut ref) $ \_ -> mr

lensMapRef ::
       forall m a b. Monad m
    => Lens' a b
    -> Ref m a
    -> Ref m b
lensMapRef l ref = let
    refGet' = fmap (\a -> getConst $ l Const a) $ refGet ref
    refPut' b = do
        a <- refGet ref
        refPut ref $ runIdentity $ l (\_ -> Identity b) a
    in MkRef refGet' refPut'

liftRef :: (MonadTrans t, Monad m) => Ref m --> Ref (t m)
liftRef (MkRef g m) = MkRef (lift g) $ \a -> lift $ m a

stateRef :: Monad m => Ref (StateT s m) s
stateRef = MkRef get put

refRunState :: Monad m => Ref m s -> StateT s m --> m
refRunState ref sm = do
    olds <- refGet ref
    (a, news) <- runStateT sm olds
    refPut ref news
    return a

ioRef :: IORef a -> Ref IO a
ioRef r = MkRef (readIORef r) (writeIORef r)

strictSTRef :: Strict.STRef s a -> Ref (Strict.ST s) a
strictSTRef r = MkRef (Strict.readSTRef r) (Strict.writeSTRef r)

lazySTRef :: Lazy.STRef s a -> Ref (Lazy.ST s) a
lazySTRef r = MkRef (Lazy.readSTRef r) (Lazy.writeSTRef r)

refParam ::
       forall m a. (MonadUnliftIO m, MonadException m)
    => Ref m a
    -> Param m a
refParam ref = let
    paramAsk = refGet ref
    paramWith :: a -> m --> m
    paramWith a mr =
        refRestore ref $ do
            refPut ref a
            mr
    in MkParam {..}

refProd ::
       forall m a. (MonadUnliftIO m, MonadException m, Monoid a)
    => Ref m a
    -> Prod m a
refProd ref = let
    prodTell a = refModify ref $ (<>) a
    prodListen :: forall r. m r -> m (r, a)
    prodListen mr =
        refRestore ref $ do
            refPut ref mempty
            r <- mr
            a <- refGet ref
            return (r, a)
    in MkProd {..}
