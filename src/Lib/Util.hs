module Lib.Util where

import           Control.Comonad
import           Control.Comonad.Cofree
import           Control.Monad.Free
import           Control.Monad.Reader
import           Data.Map               (Map)
import qualified Data.Map               as M
import           Data.Monoid
import           Data.Set               (Set)
import qualified Data.Set               as S
import           Lib.Compiler
import           Lib.Errors
import           Lib.Syntax
import           Lib.Types.Type         (Sigma)
import           Prelude                hiding (lookup)

type Defn = (Symbol, AnnotatedExpr ())
type Sig = (Symbol, Sigma)

splitUp :: [TopLevel] -> ([Defn], [Sig])
splitUp = foldl go ([], [])
    where go (defns, sigs) tl = case tl of
              Define sym defn   -> ((sym, defn):defns, sigs)
              Signature sym sig -> (defns, (sym,sig):sigs)
