{-# LANGUAGE OverloadedStrings #-}

module Lib.Parser where

import Prelude hiding (takeWhile)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Attoparsec.Text
import Control.Applicative
import Data.Char (isDigit, isAlpha)
import Control.Monad (join)
import Control.Monad.IO.Class
import Control.Monad.Free
import Control.Monad.Except
import Lib.Syntax.Surface
import Lib.Util
import Lib.Errors

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
symchars = "=<>.!@#$%^&*{}[]+-/\\"

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
    return . join $ aApp op operands

lam_parser :: Parser Text
lam_parser =  (string "\\")

typed_sym :: Parser (String, Maybe (SurfaceExpr a))
typed_sym = (parens has_type) <|> (sym >>= \s -> return (s, Nothing))
    where has_type = do
              sig@(Free (SigS sym _ _))  <- sig_parser
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
    return . join $ aFun (map fst args) body (map snd args)

if_parser :: Parser (SurfaceExpr a)
if_parser = do
    string "if"
    skipSpace
    c <- expr_parser
    skipSpace
    t <- expr_parser
    skipSpace
    e <- expr_parser
    return . join $ aIf c t e

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
    string "def"
    skipSpace
    sym <- sym
    skipSpace
    val <- expr_parser
    return . join $ aDef sym val

defun_parser :: Parser (SurfaceExpr a)
defun_parser = do
    string "defun"
    skipSpace
    name <- sym
    skipSpace
    char '('
    args <- typed_sym `sepBy` (many space)
    skipSpace
    char ')'
    skipSpace
    body <- expr_parser
    return . join $ aDef name (join $ aFun (map fst args) body
                               (map snd args))

-- | For standalone type signatures
sig_type_parser :: Parser ([(String, TypeLit)], [[TypeLit]])
sig_type_parser = (parens pred_type) <|> bare_type where

    pred_type :: Parser ([(String, TypeLit)], [[TypeLit]])
    pred_type = do
        skipSpace
        string "=>"
        skipSpace
        preds <- parens (pred `sepBy` (many space))
        skipSpace
        (_, t) <- bare_type
        skipSpace
        return (preds, t)

    pred :: Parser (String, TypeLit)
    pred = parens $ do
        skipSpace
        p <- sym
        skipSpace
        t <- ty_sym
        skipSpace
        return (p, t)

    bare_type :: Parser ([(String, TypeLit)], [[TypeLit]])
    bare_type = (parens compound_type) <|> single_type

    compound_type :: Parser ([(String, TypeLit)], [[TypeLit]])
    compound_type = do
        skipSpace
        string "->"
        skipSpace
        ts <- sig_typelit_parser `sepBy` (many space)
        skipSpace
        return ([], ts)

    single_type :: Parser ([(String, TypeLit)], [[TypeLit]])
    single_type = do
        t <- sig_typelit_parser
        return ([], [t])

ty_sym :: Parser TypeLit
ty_sym = do
    s@(c:cs) <- sym
    return $ if elem c ['a'..'z']
                 then TyVarLit s
                 else TyConLit s

sig_typelit_parser :: Parser [TypeLit]
sig_typelit_parser = do
    skipSpace
    t <- one <|> (parens more)
    skipSpace
    return t

    where
        one :: Parser [TypeLit]
        one = do
            t <- ty_sym
            return [t]

        more :: Parser [TypeLit]
        more = do
            ts <- ty_sym `sepBy` (many space)
            return ts

sig_parser :: Parser (SurfaceExpr a)
sig_parser = do
    char ':'
    skipSpace
    name <- sym
    skipSpace
    t <- sig_type_parser
    skipSpace
    return . join $ aSig name [] t

toplevel_parser :: Parser (SurfaceExpr a)
toplevel_parser = do
    skipSpace
    defn <- (parens defun_parser) <|> (parens def_parser) <|> (parens sig_parser)
    skipSpace
    return defn

module_parser :: Parser [SurfaceExpr a]
module_parser = toplevel_parser `sepBy` (many space)

parse_expr'
    :: (Monad m)
    => Text
    -> m (Either String (SurfaceExpr ()))
parse_expr' input = do
    let parse_result = parseOnly ((parens def_parser)
                                  <|> (parens defun_parser)
                                  <|> (parens sig_parser)
                                  <|> expr_parser) input
    return parse_result

parse_expr
    :: (Monad m)
    => Text
    -> ExceptT PsiloError m (SurfaceExpr ())
parse_expr t = do
    result <- parse_expr' t
    case result of
        Left err -> throwError $ ParserError err
        Right result' -> return result'

parse_multi'
    :: MonadIO m
    => Text
    -> m (Either String [SurfaceExpr ()])
parse_multi' inp = do
    let the_parser = module_parser
    let parse_result = parseOnly the_parser inp
    return parse_result

parse_multi
    :: MonadIO m
    => Text
    -> ExceptT PsiloError m [SurfaceExpr ()]
parse_multi t = do
    result <- parse_multi' t
    case result of
        Left err -> throwError $ ParserError err
        Right result' -> return result'

-- | Remove ";" comments from source code
removeComments :: Text -> Text
removeComments = Text.unlines . fmap fst . fmap breakComment . Text.lines
    where breakComment = Text.break (\c -> c == ';')
