module Control.Monad.Ology.General.Exception
    ( module Control.Monad.Ology.General.Exception.Class
    , module Control.Monad.Ology.General.Exception
    , CE.SomeException
    , CE.evaluate
    ) where

import qualified Control.Exception as CE
import Control.Monad.Ology.General.Exception.Class
import Control.Monad.Ology.General.Function
import Control.Monad.Ology.General.Trans.Hoist
import Control.Monad.Ology.General.Trans.Tunnel
import Import

-- | Run with asynchronous exceptions masked, passing an unmask function.
mask ::
       forall m b. MonadTunnelIO m
    => ((forall a. m a -> m a) -> m b)
    -> m b
mask call = tunnelIO $ \unlift -> CE.mask $ \restore -> unlift $ call $ hoistIO restore

-- | Bracket an operation with before and after operations.
-- The whole thing is masked, with the main operation unmasked.
bracket ::
       forall m a b. (MonadException m, MonadTunnelIO m)
    => m a
    -> (a -> m ())
    -> (a -> m b)
    -> m b
bracket before after thing =
    mask $ \restore -> do
        a <- before
        b <- onException (restore (thing a)) (after a)
        after a
        return b

-- | Variant of 'bracket'.
finally ::
       forall m a. (MonadException m, MonadTunnelIO m)
    => m a
    -> m ()
    -> m a
finally ma handler = bracket (return ()) (const handler) (const ma)

-- | Variant of 'bracket'.
bracket_ ::
       forall m. (MonadException m, MonadTunnelIO m)
    => m ()
    -> m ()
    -> m --> m
bracket_ before after thing = bracket before (const after) (const thing)

-- | Like 'bracket', but doesn\'t mask asynchronous exceptions.
bracketNoMask ::
       forall m a b. MonadException m
    => m a
    -> (a -> m ())
    -> (a -> m b)
    -> m b
bracketNoMask before after thing = do
    a <- before
    b <- onException (thing a) (after a)
    after a
    return b

-- | Variant of 'bracketNoMask'.
bracketNoMask_ ::
       forall m. MonadException m
    => m ()
    -> m ()
    -> m --> m
bracketNoMask_ before after thing = bracketNoMask before (const after) (const thing)

-- | Like 'bracketNoMask', but doesn\'t catch any exceptions.
bracketFake ::
       forall m a b. Monad m
    => m a
    -> (a -> m ())
    -> (a -> m b)
    -> m b
bracketFake before after thing = do
    a <- before
    b <- thing a
    after a
    return b
