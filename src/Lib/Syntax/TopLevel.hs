module Lib.Syntax.TopLevel where

import           Data.Map             (Map)
import qualified Data.Map             as M
import           Data.Monoid
import           Lib.Syntax.Annotated
import           Lib.Syntax.Core
import           Lib.Syntax.Symbol
import           Lib.Types.Scheme
import           Lib.Types.Type       (Sigma, TyVar (..), Type (..))

data TopLevel = TopLevel
    { definitions :: Map Symbol (AnnotatedExpr (Maybe Type))
    , signatures  :: Map Symbol Sigma
    , typedefs    :: Map Symbol ([TyVar], Sigma)
    }

instance Monoid TopLevel where
    mempty = TopLevel mempty mempty mempty
    (TopLevel aD aS aT) `mappend` (TopLevel bD bS bT) = TopLevel {
        definitions = (aD `mappend` bD),
        signatures = (aS `mappend` bS),
        typedefs = (aT `mappend` bT)
        }
