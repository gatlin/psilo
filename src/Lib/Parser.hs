{-# LANGUAGE OverloadedStrings #-}

module Lib.Parser where

import           Control.Applicative
import           Control.Monad        (join)
import           Control.Monad.Except
import           Control.Monad.Free
import           Data.Attoparsec.Text
import           Data.Char            (isAlpha, isDigit, ord)
import           Data.List            (nub)
import qualified Data.Set             as S
import           Data.Text            (Text)
import qualified Data.Text            as Text
import           Lib.Compiler
import           Lib.Errors
import           Lib.Syntax.Surface
import           Lib.Types.Frame
import           Lib.Types.Kind
import           Lib.Types.Scheme
import           Lib.Types.Type
import           Lib.Types.Type       (tyFun)
import           Prelude              hiding (takeWhile)

-- String hashing, for getting a unique Int from type variables
string_hash :: String -> Int
string_hash = foldl (\hashVal ch -> 31 * hashVal + (ord ch)) 0

num_parser :: Parser (SurfaceExpr a)
num_parser = do
    whole_part <- many1 digit
    mDot <- peekChar
    case mDot of
        Just '.' -> do
            char '.'
            frac_part <- many1 digit
            return $ aFloat $ read $ whole_part ++ ['.'] ++ frac_part
        _ -> return $ aInt (read whole_part)

symchars :: String
symchars = "~=<>.?!@#$%^&*{}[]+-/_"

symchar :: Parser Char
symchar = satisfy $ \c -> elem c symchars

sym :: Parser String
sym = do
    firstChar <- letter <|> symchar
    otherChars <- many' $ letter <|> digit <|> symchar
    return $ firstChar:otherChars

id_parser :: Parser (SurfaceExpr a)
id_parser = do
    the_sym <- sym
    return $ aId the_sym

{-
string_parser :: Parser (CoreExpr a)
string_parser = do
    char '"'
    str <- many' $ notChar '"'
    char '"'
    Return $ aString $ Text.pack str
-}

bool_parser :: Parser (SurfaceExpr a)
bool_parser = do
    char '#'
    bv <- char 't' <|> char 'f'
    case bv of
        't' -> return $ aBool True
        'f' -> return $ aBool False

app_parser :: Parser (SurfaceExpr a)
app_parser = do
    (op:operands) <- expr_parser `sepBy1` (many space)
    return $ aApp op operands

lam_parser :: Parser Text
lam_parser =  (string "\\")

typed_sym :: Parser (String, Maybe (SurfaceExpr a))
typed_sym = (parens has_type) <|> (sym >>= \s -> return (s, Nothing))
    where has_type = do
              sig@(Free (SigS sym _))  <- sig_parser
              return (sym, Just sig)

fun_parser :: Parser (SurfaceExpr a)
fun_parser = do
    char '\\'
    skipSpace
    char '('
    skipSpace
    args <- typed_sym `sepBy` (many space)
    skipSpace
    char ')'
    skipSpace
    body <- expr_parser
    return $ aFun (map fst args) body (map snd args)

if_parser :: Parser (SurfaceExpr a)
if_parser = do
    string "if"
    skipSpace
    c <- expr_parser
    skipSpace
    t <- expr_parser
    skipSpace
    e <- expr_parser
    return $ aIf c t e

parens :: Parser a -> Parser a
parens p = do
    char '('
    skipSpace
    thing <- p
    skipSpace
    char ')'
    return thing

expr_parser :: Parser (SurfaceExpr a)
expr_parser = (parens fun_parser)
          <|> (parens if_parser)
          <|> bool_parser
          <|> id_parser
          <|> num_parser
--          <|> string_parser
          <|> (parens app_parser)

def_parser :: Parser (SurfaceExpr a)
def_parser = do
    string "="
    skipSpace
    sym <- sym
    skipSpace
    val <- expr_parser
    return $ aDef sym val

defun_parser :: Parser (SurfaceExpr a)
defun_parser = do
    string "="
    skipSpace
    name <- sym
    skipSpace
    char '('
    skipSpace
    args <- typed_sym `sepBy` (many space)
    skipSpace
    char ')'
    skipSpace
    body <- expr_parser
    return $ aDef name (join $ aFun (map fst args) body
                               (map snd args))

typedef_parser :: Parser (SurfaceExpr a)
typedef_parser = do
    string "::"
    skipSpace
    name <- sym
    skipSpace
    vars <- parens (sym `sepBy` (many space))
    vars' <- forM vars $ \var ->
        return (TyVar (string_hash var) Star)
    skipSpace
    body <- scheme_parser <|> void_typedef_body
    skipSpace
    return $ aTypeDef name vars' body

void_typedef_body :: Parser Type
void_typedef_body = skipSpace >> return (TList [])

class_sig_parser :: String -> Type -> Parser (SurfaceExpr a)
class_sig_parser className vars = do
    (Free (SigS name t)) <- sig_parser
    return $ aSig name ([IsIn className vars] :=> t)

{-
classdef_vars = (parens (pred_type class_vars)) <|> class_vars'
    where class_vars = parens $ do
              vs <- ty_sym `sepBy` (many space)
              return $ TList vs

          class_vars' = do
              cvs <- class_vars
              return $ [] :=> cvs
-}

classdef_vars = (parens (pred_type class_vars)) <|> class_vars
    where class_vars = do
              vs <- (parens several_vars) <|> ty_sym
              return $ [] :=> vs

          several_vars = do
              vs <- ty_sym `sepBy` (many space)
              return $ TList vs

classdef_parser :: Parser (SurfaceExpr a)
classdef_parser = do
    string "@:"
    skipSpace
    name <- sym
    skipSpace
    (supers :=> ty) <- classdef_vars
    supers' <- forM supers $ \(IsIn super _) -> return super
    skipSpace
    body <- ((parens (class_sig_parser name ty)) <|>
             (parens defun_parser)) `sepBy` (many space)
    skipSpace
    return $ aClassDef name supers' ty body

classinst_parser :: Parser (SurfaceExpr a)
classinst_parser = do
    string "@="
    skipSpace
    name <- sym
    skipSpace
--    ty <- parens $ ty_sym `sepBy` (many space)
    (supers :=> ty) <- classdef_vars
    supers' <- forM supers $ \(IsIn super _) -> return super
    skipSpace
    defns <- (parens defun_parser) `sepBy` (many space)
    return $ aClassInst name supers' ty defns

scheme_parser :: Parser Type
scheme_parser = sigma

sigma :: Parser Type
sigma = (parens quantified) <|> unquantified

quantified :: Parser Type
quantified = do
    skipSpace
    string "forall"
    skipSpace
    vars <- parens (sym `sepBy` (many space))
    skipSpace
    t <- unquantified
    skipSpace
    return $ TForall (fmap (\v -> TyVar (string_hash v) Star) vars) t

unquantified :: Parser Type
unquantified = do
    skipSpace
    t <- (parens $ pred_type rho) <|> rho
    skipSpace
    return $ t

rho :: Parser Type
rho = (parens sigma_arrow) <|> tau

sigma_arrow :: Parser Type
sigma_arrow = do
    skipSpace
    string "->"
    skipSpace
    tys <- sigma `sepBy` (many space)
    skipSpace
    return $ TList $ tyFun : tys

tau :: Parser Type
tau = ty_sym <|> (parens compound)

compound :: Parser Type
compound = do
        skipSpace
        ts <- tau `sepBy` (many space)
        skipSpace
        return $ TList ts


ty_sym :: Parser Type
ty_sym = do
    s@(c:cs) <- sym
    return $ if (elem c ['a'..'z'])
                then TVar (TyVar (string_hash s) Star)
                else TSym (TyLit s Star)


pred_type :: Parser Type -> Parser Type
pred_type p = do
    skipSpace
    string "=>"
    skipSpace
    preds <- parens (Lib.Parser.pred `sepBy` (many space))
    skipSpace
    t <- p
    skipSpace
    return $ (preds :=> t)

pred :: Parser Pred
pred = parens $ do
    skipSpace
    p <- sym
    skipSpace
    t <- ty_sym
    skipSpace
    return $ IsIn p t

sig_parser :: Parser (SurfaceExpr a)
sig_parser = do
    char ':'
    skipSpace
    name <- sym
    skipSpace
    t <- scheme_parser
    skipSpace
    return $ aSig name t where

toplevel_parser :: Parser (SurfaceExpr a)
toplevel_parser = do
    skipSpace
    defn <- (parens defun_parser)
        <|> (parens def_parser)
        <|> (parens sig_parser)
        <|> (parens typedef_parser)
        <|> (parens classdef_parser)
        <|> (parens classinst_parser)
    skipSpace
    return defn

module_parser :: Parser [SurfaceExpr a]
module_parser = toplevel_parser `sepBy` (many space)

parse_expr'
    :: (Monad m)
    => Text
    -> m (Either String (SurfaceExpr ()))
parse_expr' input = do
    let parse_result = parseOnly ((parens defun_parser)
                                  <|> (parens def_parser)
                                  <|> (parens sig_parser)
                                  <|> (parens typedef_parser)
                                  <|> (parens classdef_parser)
                                  <|> (parens classinst_parser)
                                  <|> expr_parser) input
    return parse_result

parse_expr
    :: Text
    -> Compiler (SurfaceExpr ())
parse_expr t = do
    result <- parse_expr' t
    case result of
        Left err      -> throwError $ ParserError err
        Right result' -> return result'

parse_multi' inp = do
    let the_parser = module_parser
    let parse_result = parseOnly the_parser inp
    return parse_result

parse_multi
    :: Text
    -> Compiler [SurfaceExpr ()]
parse_multi t = do
    result <- parse_multi' t
    case result of
        Left err      -> throwError $ ParserError err
        Right result' -> return result'

-- | Remove ";" comments from source code
removeComments :: Text -> Text
removeComments = Text.unlines . fmap fst . fmap breakComment . Text.lines
    where breakComment = Text.break (\c -> c == ';')
