module Lib.Syntax.TopLevel where

import           Data.Map             (Map)
import qualified Data.Map             as M
import           Data.Monoid
import           Data.Set             (Set)
import qualified Data.Set             as S
import           Lib.Syntax.Annotated
import           Lib.Syntax.Core
import           Lib.Syntax.Symbol
import           Lib.Types.Class
import           Lib.Types.Scheme
import           Lib.Types.Type       (Sigma, TyVar (..), Type (..))

data TopLevel = TopLevel
    { definitions :: Map Symbol (AnnotatedExpr (Maybe Type))
    , signatures  :: Map Symbol Sigma
    , typedefs    :: Map Symbol ([TyVar], Sigma)
    , classes     :: EnvTransformer
    , methods     :: Map Symbol (Set (Sigma, AnnotatedExpr (Maybe Type)))
    }

instance Monoid TopLevel where
    mempty = TopLevel mempty mempty mempty mempty mempty
    (TopLevel aD aS aT aC aM) `mappend` (TopLevel bD bS bT bC bM) = TopLevel {
        definitions = (aD `mappend` bD),
        signatures = (aS `mappend` bS),
        typedefs = (aT `mappend` bT),
        classes = (aC `mappend` bC),
        methods = (aM `mappend` bM)
        }
