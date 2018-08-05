module Lib.Syntax.TopLevel where

import           Data.Map             (Map)
import qualified Data.Map             as M
import           Data.Monoid
import           Lib.Syntax.Annotated
import           Lib.Syntax.Core
import           Lib.Syntax.Symbol
import           Lib.Types.Class      (Class (..), ClassEnv (..),
                                       EnvTransformer (..))
import           Lib.Types.Scheme
import           Lib.Types.Type       (Sigma, TyVar (..), Type (..))

data TopLevel = TopLevel
    { definitions :: Map Symbol (AnnotatedExpr (Maybe Type))
    , signatures  :: Map Symbol Sigma
    , typedefs    :: Map Symbol ([TyVar], Sigma)
    -- for now we ignore default implementations
    , classdefs   :: EnvTransformer
    }


instance Monoid TopLevel where
    mempty = TopLevel mempty mempty mempty mempty
    (TopLevel aD aS aT aC) `mappend` (TopLevel bD bS bT bC) = TopLevel {
        definitions = (aD `mappend` bD),
        signatures = (aS `mappend` bS),
        typedefs = (aT `mappend` bT),
        classdefs = (aC `mappend` bC)
        }
