module Lib.Util where

import Prelude hiding (lookup)
import Lib.Syntax
import Data.Set (Set)
import qualified Data.Set as S
import Data.Map (Map)
import qualified Data.Map as M
import Data.Monoid
import Control.Monad.Free
import Control.Monad.Reader
import Control.Comonad
import Control.Comonad.Cofree
