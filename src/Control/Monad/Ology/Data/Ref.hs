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
    { getD :: m a
    , putD :: a -> m ()
    }

modifyD :: Monad m => Ref m a -> (a -> a) -> m ()
modifyD ref f = do
    a <- getD ref
    putD ref $ f a

restoreD :: (MonadUnliftIO m, MonadException m) => Ref m a -> m --> m
restoreD ref mr = bracket (getD ref) (putD ref) $ \_ -> mr

lensMapRef ::
       forall m a b. Monad m
    => Lens' a b
    -> Ref m a
    -> Ref m b
lensMapRef l ref = let
    getD' = fmap (\a -> getConst $ l Const a) $ getD ref
    putD' b = do
        a <- getD ref
        putD ref $ runIdentity $ l (\_ -> Identity b) a
    in MkRef getD' putD'

unitRef :: Applicative m => Ref m ()
unitRef = MkRef (pure ()) (\_ -> pure ())

pairRef :: Applicative m => Ref m a -> Ref m b -> Ref m (a, b)
pairRef ra rb = MkRef (liftA2 (,) (getD ra) (getD rb)) $ \(a, b) -> putD ra a *> putD rb b

liftRef :: (MonadTrans t, Monad m) => Ref m --> Ref (t m)
liftRef (MkRef g m) = MkRef (lift g) $ \a -> lift $ m a

stateRef :: Monad m => Ref (StateT s m) s
stateRef = MkRef get put

refRunState :: Monad m => Ref m s -> StateT s m --> m
refRunState ref sm = do
    olds <- getD ref
    (a, news) <- runStateT sm olds
    putD ref news
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
    askD = getD ref
    withD :: a -> m --> m
    withD a mr =
        restoreD ref $ do
            putD ref a
            mr
    in MkParam {..}

refProd ::
       forall m a. (MonadUnliftIO m, MonadException m, Monoid a)
    => Ref m a
    -> Prod m a
refProd ref = let
    tellD a = modifyD ref $ (<>) a
    listenD :: forall r. m r -> m (r, a)
    listenD mr =
        restoreD ref $ do
            putD ref mempty
            r <- mr
            a <- getD ref
            return (r, a)
    in MkProd {..}
