{-# OPTIONS -fno-warn-orphans #-}

module Control.Monad.Ology.MonadInner where

import Control.Monad.Ology.Result
import Import

-- | Instances of this type are isomorphic to @Either P (Q,a)@ for some types @P@ and @Q@ (with @Monoid Q@).
-- Must satisfy:
-- * @retrieveInner (fmap f w) = fmap f (retrieveInner w)@
-- * @case (retrieveInner w) of {Left w' -> w';Right a -> fmap (\_ -> a) w;} = w@
class (Traversable m, Monad m) => MonadInner m where
    retrieveInner :: forall a. m a -> Result (m Void) a

instance MonadInner Identity where
    retrieveInner (Identity a) = SuccessResult a

instance MonadInner Maybe where
    retrieveInner (Just a) = SuccessResult a
    retrieveInner Nothing = FailureResult Nothing

instance MonadInner (Either p) where
    retrieveInner (Right b) = SuccessResult b
    retrieveInner (Left a) = FailureResult $ Left a

instance Monoid p => MonadInner ((,) p) where
    retrieveInner (_, a) = SuccessResult a

instance MonadInner (Result e) where
    retrieveInner (SuccessResult a) = SuccessResult a
    retrieveInner (FailureResult e) = FailureResult (FailureResult e)

instance MonadInner m => MonadInner (IdentityT m) where
    retrieveInner (IdentityT ma) = mapResultFailure IdentityT $ retrieveInner ma

instance (MonadInner m, Monoid w) => MonadInner (WriterT w m) where
    retrieveInner (WriterT maw) =
        case retrieveInner maw of
            SuccessResult (a, _) -> SuccessResult a
            FailureResult mv -> FailureResult $ WriterT $ fmap absurd mv

instance MonadInner m => MonadInner (ExceptT e m) where
    retrieveInner (ExceptT meea) =
        case retrieveInner meea of
            SuccessResult (Right a) -> SuccessResult a
            SuccessResult (Left e) -> FailureResult $ ExceptT $ fmap (\_ -> Left e) meea
            FailureResult mv -> FailureResult $ ExceptT $ fmap Right mv

instance MonadInner m => MonadInner (MaybeT m) where
    retrieveInner (MaybeT mma) =
        case retrieveInner mma of
            SuccessResult (Just a) -> SuccessResult a
            SuccessResult Nothing -> FailureResult $ MaybeT $ fmap (\_ -> Nothing) mma
            FailureResult mv -> FailureResult $ MaybeT $ fmap Just mv

mToMaybe ::
       forall m a. MonadInner m
    => m a
    -> Maybe a
mToMaybe = resultToMaybe . retrieveInner

commuteInner ::
       forall m f a. (MonadInner m, Applicative f)
    => m (f a)
    -> f (m a)
commuteInner mfa =
    case retrieveInner mfa of
        SuccessResult fa -> fmap pure fa
        FailureResult mv -> pure $ fmap absurd mv
