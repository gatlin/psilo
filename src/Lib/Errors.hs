module Lib.Errors where

import           Lib.Syntax.Symbol
import           Lib.Types.Type    (TyVar (..), Type (..))

-- Basically every kind of error message will go in here so we can have one big
-- Except/ExceptT jamboree throughout the whole project

data PsiloError
    -- type inference
    = UnificationFail Type Type
    | UnificationMismatch [Type] [Type]
    | TypeMismatch Type Type
    | InfiniteType TyVar Type
    | UnboundVariable Symbol
    | OtherTypeError String
    -- type classes
    | ClassAlreadyDefined Symbol
    | SuperclassNotDefined Symbol
    | NoClassForInstance Symbol Symbol
    | OverlappingInstance Symbol
    -- parsing
    | ParserError String
    -- preprocessor
    | PreprocessError String
    | OtherError String
    deriving (Eq)

instance Show PsiloError where
    show (UnificationFail t1 t2) =
        "Unification failure for types: " ++
        (show t1) ++ " and " ++ (show t2)

    show (UnificationMismatch ts1 ts2) =
        "Unification mismatch for types: " ++
        (show ts1) ++ " and " ++ (show ts2)

    show (TypeMismatch t1 t2) =
        "Failed to match types: " ++
        (show t1) ++ " and " ++ (show t2)

    show (InfiniteType tv ty) =
        "Infinite type: " ++ (show tv) ++ " ~ " ++ (show ty)

    show (UnboundVariable sym) = "Unbound variable: " ++ sym

    show (OtherTypeError errMsg) =
        "Other type error: " ++ errMsg

    show (ClassAlreadyDefined sym) =
        "Duplicate class definition: " ++ sym

    show (SuperclassNotDefined sym) =
        "Undefined super class: " ++ sym

    show (NoClassForInstance klass ty) =
        "No class instance of " ++ klass ++
        " for type " ++ (show ty)

    show (OverlappingInstance sym) = "Overlapping instance for " ++ sym

    show (ParserError errMsg) = "Parser error: " ++ errMsg

    show (PreprocessError errMsg) = "Preprocessor error: " ++ errMsg

    show (OtherError errMsg) = errMsg
