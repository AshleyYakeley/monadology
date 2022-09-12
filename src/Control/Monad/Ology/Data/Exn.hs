module Control.Monad.Ology.Data.Exn where

import Control.Monad.Ology.General
import Control.Monad.Ology.Specific.Result
import Import

-- | Exceptions that can be thrown and caught in this monad.
type Exn :: (Type -> Type) -> Type -> Type
data Exn m e = MkExn
    { exnThrow :: forall a. e -> m a
    , exnCatch :: forall a. m a -> (e -> m a) -> m a
    }

instance Invariant (Exn m) where
    invmap f g (MkExn t c) = MkExn (t . g) (\ma ema -> c ma $ ema . f)

instance Summable (Exn m) where
    rVoid = MkExn {exnThrow = absurd, exnCatch = \m _ -> m}
    exn1 <+++> exn2 =
        MkExn
            { exnThrow = either (exnThrow exn1) (exnThrow exn2)
            , exnCatch = \m k -> exnCatch exn1 (exnCatch exn2 m (k . Right)) (k . Left)
            }

exnTry :: Monad m => Exn m e -> m a -> m (Result e a)
exnTry exn ma = exnCatch exn (fmap SuccessResult ma) $ \e -> return $ FailureResult e

exnHandle :: Exn m e -> (e -> m a) -> m a -> m a
exnHandle exn handler ma = exnCatch exn ma handler

exnOnException ::
       forall e m a. Monad m
    => Exn m e
    -> m a
    -> m ()
    -> m a
exnOnException exn ma handler = exnCatch exn ma $ \e -> handler >> exnThrow exn e

exnBracket ::
       forall e m a b. MonadTunnelIO m
    => Exn m e
    -> m a
    -> (a -> m ())
    -> (a -> m b)
    -> m b
exnBracket exn before after thing =
    mask $ \restore -> do
        a <- before
        r <- exnOnException exn (restore (thing a)) (after a)
        _ <- after a
        return r

exnFinally ::
       forall e m a. MonadTunnelIO m
    => Exn m e
    -> m a
    -> m ()
    -> m a
exnFinally exn ma handler = exnBracket exn (return ()) (const handler) (const ma)

exnBracket_ ::
       forall e m. MonadTunnelIO m
    => Exn m e
    -> m ()
    -> m ()
    -> m --> m
exnBracket_ exn before after thing = exnBracket exn before (const after) (const thing)

mapExn :: (e2 -> e1) -> (e1 -> Maybe e2) -> Exn m e1 -> Exn m e2
mapExn f g exn =
    MkExn
        { exnThrow = exnThrow exn . f
        , exnCatch =
              \ma handler ->
                  exnCatch exn ma $ \e ->
                      case g e of
                          Nothing -> exnThrow exn e
                          Just e' -> handler e'
        }

liftExn ::
       forall t m. (MonadTransTunnel t, Monad m)
    => Exn m --> Exn (t m)
liftExn (MkExn t c :: Exn m e) = let
    t' :: forall a. e -> t m a
    t' e = lift $ t e
    c' :: forall a. t m a -> (e -> t m a) -> t m a
    c' tma handler = tunnel $ \unlift -> c (unlift tma) $ \e -> unlift $ handler e
    in MkExn t' c'

allExn ::
       forall m. MonadException m
    => Exn m (Exc m)
allExn = MkExn throwExc catchExc

someExn ::
       forall e m. MonadCatch e m
    => Exn m e
someExn = MkExn throw catch
