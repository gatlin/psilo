module Lib.Types.TypeCheck where

import           Lib.Syntax.Annotated
import           Lib.Syntax.Core
import           Lib.Syntax.Symbol

import           Lib.Types.Constraint
import           Lib.Types.Frame
import           Lib.Types.Kind
import           Lib.Types.Scheme
import           Lib.Types.Type
import           Lib.Types.TypeEnv

import           Lib.Compiler
import           Lib.Errors

import           Data.List              (nub)
import           Data.Map               (Map)
import qualified Data.Map               as M
import           Data.Set               (Set)
import qualified Data.Set               as S

import           Control.Monad.Except
import           Control.Monad.Reader
import           Control.Monad.State
import           Control.Monad.Writer
import           Data.Functor.Identity

import           Control.Comonad
import           Control.Comonad.Cofree

data TypeCheckState = TypeCheckState
    { varCount    :: Int
    , frame       :: Frame
    , constraints :: [Constraint]
    , preds       :: [Pred]
    } deriving (Eq, Show)

initTypeCheckState :: TypeCheckState
initTypeCheckState = TypeCheckState 0 mempty mempty mempty

type TypeCheck =
    ReaderT TypeEnv (
    StateT TypeCheckState (WriterT [Constraint] Compiler))

-- I know it isn't pretty
logTypeCheck :: String -> TypeCheck ()
logTypeCheck = lift . lift . lift . logMsg

fresh :: Kind -> TypeCheck TyVar
fresh k = do
    c <- gets varCount
    modify $ \st -> st { varCount = c + 1 }
    return $ TyVar c k
