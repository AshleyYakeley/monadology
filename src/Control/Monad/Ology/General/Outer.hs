module Control.Monad.Ology.General.Outer where

import Control.Monad.Ology.General.Function
import Import

-- | Monads that can compose as the outer monad with any inner monad to make a monad.
-- See 'Control.Monad.Ology.Specific.ComposeOuter.ComposeOuter'.
-- Instances of this type are isomorphic to @P -> a@ for some type @P@.
--
-- Must satisfy:
--
-- * @fmap (\\ex -> unWExtract ex ma) getExtract = ma@.
class Monad m => MonadOuter m where
    getExtract :: m (WExtract m)

instance MonadOuter Identity where
    getExtract = return $ MkWExtract runIdentity

instance MonadOuter ((->) r) where
    getExtract r = MkWExtract $ \ra -> ra r

commuteOuter ::
    forall m f a.
    (MonadOuter m, Functor f) =>
    f (m a) ->
    m (f a)
commuteOuter fma = do
    MkWExtract ext <- getExtract
    return $ fmap ext fma
