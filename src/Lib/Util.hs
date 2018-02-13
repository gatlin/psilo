module Lib.Util where

import Prelude hiding (lookup)
import Lib.Syntax
import Lib.Types.Scheme (Scheme)
import Lib.Compiler
import Lib.Errors
import Data.Set (Set)
import qualified Data.Set as S
import Data.Map (Map)
import qualified Data.Map as M
import Data.Monoid
import Control.Monad.Free
import Control.Monad.Reader
import Control.Comonad
import Control.Comonad.Cofree

type Defn = (Symbol, AnnotatedExpr ())
type Sig = (Symbol, Scheme)

splitUp :: [TopLevel] -> ([Defn], [Sig])
splitUp = foldl go ([], [])
    where go (defns, sigs) tl = case tl of
              Define sym defn -> ((sym, defn):defns, sigs)
              Signature sym sig -> (defns, (sym,sig):sigs)
