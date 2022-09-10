module Control.Monad.Ology.General.Function where

import Control.Monad.Ology.General.IO
import Control.Monad.Ology.General.Trans.Trans
import Import

-- type (-->) :: forall k. (k -> Type) -> (k -> Type) -> Type
type p --> q = forall a. p a -> q a

type Raised :: forall k. (k -> Type) -> (k -> Type) -> Type
newtype Raised p q = MkRaised
    { runRaised :: p --> q
    }

wLift :: (MonadTrans t, Monad m) => Raised m (t m)
wLift = MkRaised lift

wLiftIO :: MonadIO m => Raised IO m
wLiftIO = MkRaised liftIO

instance Category Raised where
    id = MkRaised id
    (MkRaised bc) . (MkRaised ab) = MkRaised $ bc . ab

-- type (-/->) :: forall k. (k -> Type) -> (k -> Type) -> Type
type ma -/-> mb = forall r. ((mb --> ma) -> ma r) -> mb r

mBackraisedToRaised :: (ma -/-> mb) -> ma --> mb
mBackraisedToRaised mbf ma = mbf $ \_ -> ma

type Backraised :: forall k. (k -> Type) -> (k -> Type) -> Type
newtype Backraised p q = MkBackraised
    { runBackraised :: p -/-> q
    }

instance Category Backraised where
    id = MkBackraised $ \f -> f id
    (MkBackraised bc) . (MkBackraised ab) = MkBackraised $ \f -> bc $ \mcmb -> ab $ \mbma -> f $ mbma . mcmb
