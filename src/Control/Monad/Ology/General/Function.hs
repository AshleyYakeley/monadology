module Control.Monad.Ology.General.Function
    ( TransKind
    -- * Raised
    , Raised
    , type (-->)
    , WRaised(..)
    , wLift
    , wLiftIO
    -- * Backraised
    , Backraised
    , type (-/->)
    , backraisedToRaised
    , WBackraised(..)
    , wBackraisedToWRaised
    -- * Unlift
    , Unlift
    , WUnlift(..)
    , wUnliftToWRaised
    -- * Extract
    , Extract
    , WExtract(..)
    ) where

import Control.Monad.Ology.General.IO
import Control.Monad.Ology.General.Trans.Trans
import Import

type TransKind = (Type -> Type) -> (Type -> Type)

type Raised :: forall k. (k -> Type) -> (k -> Type) -> Type
type Raised p q = forall a. p a -> q a

type p --> q = Raised p q

type WRaised :: forall k. (k -> Type) -> (k -> Type) -> Type
newtype WRaised p q = MkWRaised
    { unWRaised :: p --> q
    }

wLift :: (MonadTrans t, Monad m) => WRaised m (t m)
wLift = MkWRaised lift

wLiftIO :: MonadIO m => WRaised IO m
wLiftIO = MkWRaised liftIO

instance Category WRaised where
    id = MkWRaised id
    (MkWRaised bc) . (MkWRaised ab) = MkWRaised $ bc . ab

type Backraised :: forall k. (k -> Type) -> (k -> Type) -> Type
type Backraised ma mb = forall r. ((mb --> ma) -> ma r) -> mb r

type ma -/-> mb = Backraised ma mb

backraisedToRaised :: (ma -/-> mb) -> ma --> mb
backraisedToRaised mbf ma = mbf $ \_ -> ma

type WBackraised :: forall k. (k -> Type) -> (k -> Type) -> Type
newtype WBackraised p q = MkWBackraised
    { unWBackraised :: p -/-> q
    }

instance Category WBackraised where
    id = MkWBackraised $ \f -> f id
    (MkWBackraised bc) . (MkWBackraised ab) = MkWBackraised $ \f -> bc $ \mcmb -> ab $ \mbma -> f $ mbma . mcmb

wBackraisedToWRaised :: WBackraised ma mb -> WRaised ma mb
wBackraisedToWRaised (MkWBackraised f) = MkWRaised $ backraisedToRaised f

type Unlift :: ((Type -> Type) -> Constraint) -> TransKind -> Type
type Unlift c t = forall (m :: Type -> Type). c m => t m --> m

type WUnlift :: ((Type -> Type) -> Constraint) -> TransKind -> Type
newtype WUnlift c t = MkWUnlift
    { unWUnlift :: Unlift c t
    }

wUnliftToWRaised :: c m => WUnlift c t -> WRaised (t m) m
wUnliftToWRaised (MkWUnlift unlift) = MkWRaised unlift

type Extract :: (Type -> Type) -> Type
type Extract m = forall a. m a -> a

type WExtract :: (Type -> Type) -> Type
newtype WExtract m = MkWExtract
    { unWExtract :: Extract m
    }
