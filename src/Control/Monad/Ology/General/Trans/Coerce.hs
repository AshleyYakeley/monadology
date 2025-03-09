module Control.Monad.Ology.General.Trans.Coerce where

import Control.Monad.Ology.General.Trans.Trans
import Import

-- | A monad transformer for which coercibility is transitive.
class MonadTrans t => MonadTransCoerce t where
    transCoerce ::
        forall m1 m2.
        Coercible m1 m2 =>
        Dict (Coercible (t m1) (t m2))
