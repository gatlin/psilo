module Lib.Syntax.TopLevel where

import qualified Data.List            as L
import           Data.Map.Strict      (Map)
import qualified Data.Map.Strict      as M
import           Data.Monoid
import           Data.Set             (Set)
import qualified Data.Set             as S
import           Lib.Syntax.Annotated
import           Lib.Syntax.Core
import           Lib.Syntax.Symbol
import           Lib.Types.Class
import           Lib.Types.Scheme
import           Lib.Types.Type       (Sigma, TyVar (..), Type (..))
import           Text.Show.Unicode

data TopLevel = TopLevel
    { definitions :: Map Symbol (AnnotatedExpr (Maybe Type))
    , signatures  :: Map Symbol Sigma
    , typedefs    :: Map Symbol ([TyVar], Sigma, Bool)
    , classes     :: EnvTransformer
    , methods     :: Map Symbol (Set (AnnotatedExpr (Maybe Type)))
    }

instance Monoid TopLevel where
    mempty = TopLevel mempty mempty mempty mempty mempty
    tlA `mappend` tlB = TopLevel {
        definitions = (definitions tlA) `mappend` (definitions tlB),
        signatures = (signatures tlA) `mappend` (signatures tlB),
        typedefs = (typedefs tlA) `mappend` (typedefs tlB),
        classes = (classes tlA) `mappend` (classes tlB),
        methods = (methods tlA) `mappend` (methods tlB)
        }

-- | I look at debug output enough to warrant this.
showSignatures :: Map Symbol Sigma -> String
showSignatures sigs = L.intercalate "\n" $ map row sigs' where
    sigs' = M.toList sigs
    row (sym, sig) = sym ++ " : " ++ (ushow sig)
