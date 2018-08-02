module Lib.Syntax.TopLevel where

import           Data.Map             (Map)
import qualified Data.Map             as M
import           Data.Monoid
import           Lib.Syntax.Annotated
import           Lib.Syntax.Core
import           Lib.Syntax.Symbol
import           Lib.Types.Scheme
import           Lib.Types.Type       (Sigma, TyVar (..), Type (..))

-- | Top level syntax forms
-- Eventually we want to support the following:
--   Functions
--   Type signatures
--   Class Definitions
--   Class Instances
--   Type aliases
--   New data types


data TopLevel = TopLevel
    { definitions :: Map Symbol (AnnotatedExpr ())
    , signatures  :: Map Symbol Sigma
    , typedefs    :: Map Symbol ([TyVar], Sigma)
    -- for now we ignore default implementations
    , classdefs   :: Map Symbol (Type, Map Symbol Sigma)
    } deriving (Show)


instance Monoid TopLevel where
    mempty = TopLevel mempty mempty mempty mempty
    (TopLevel aD aS aT aC) `mappend` (TopLevel bD bS bT bC) = TopLevel {
        definitions = (aD `mappend` bD),
        signatures = (aS `mappend` bS),
        typedefs = (aT `mappend` bT),
        classdefs = (aC `mappend` bC)
        }
{-
data TopLevel
    = Define Symbol (AnnotatedExpr ()) -- ^ A value definition
    | Signature Symbol Sigma -- ^ A top level type scheme
    | Typedef Symbol [TyVar] Sigma
    deriving (Show, Eq)
-}
