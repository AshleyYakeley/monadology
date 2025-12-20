{-# OPTIONS -fno-warn-orphans #-}
{-# OPTIONS -Wno-redundant-constraints #-}
module Import
    ( module I
    )
where

import Control.Applicative as I
import Control.Category as I
import Control.Concurrent as I
import Control.Monad as I hiding (fail)
import Control.Monad.Fail as I
import Control.Monad.Fix as I
import Data.Bifunctor as I
import Data.Coerce as I
import Data.Constraint as I hiding (trans)
import Data.Foldable as I
import Data.Functor.Compose as I
import Data.Functor.Identity as I
import Data.Functor.Invariant as I
import Data.IORef as I
import Data.Kind as I
import Data.List.NonEmpty
import Data.Maybe as I
import Data.Monoid as I
import Data.Semigroup.Commutative as I
import Data.String as I (IsString (..))
import Data.Subsingular
import Data.Traversable as I
import Data.Tuple as I
import Data.Type.Witness as I
import Data.TypeRig as I
import Data.Void as I
import Prelude as I hiding (fail, id, (.))

instance Subsingular a => Commutative (Endo a)

instance Subsingular a => Commutative [a]

instance Subsingular a => Commutative (NonEmpty a)
