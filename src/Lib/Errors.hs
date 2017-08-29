module Lib.Errors where

import Lib.Syntax.Symbol
import Lib.Types.Type (Type(..), TyVar(..))

-- Basically every kind of error message will go in here so we can have one big
-- Except/ExceptT jamboree throughout the whole project

data PsiloError
    -- type inference
    = UnificationFail Type Type
    | UnificationMismatch [Type] [Type]
    | InfiniteType TyVar Type
    | UnboundVariable Symbol
    | OtherTypeError String
    -- type classes
    | ClassAlreadyDefined Symbol
    | SuperclassNotDefined Symbol
    | NoClassForInstance Symbol
    | OverlappingInstance Symbol
    -- parsing
    | ParserError String
    -- preprocessor
    | PreprocessError String
    deriving (Eq, Show)
