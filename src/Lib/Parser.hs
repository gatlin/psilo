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
import Lib.Syntax.Surface
import Lib.Util

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
    return $ aString $ Text.pack str
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

fun_parser :: Parser (SurfaceExpr a)
fun_parser = do
    char '\\'
    skipSpace
    char '('
    skipSpace
    args <- sym `sepBy` (many space)
    skipSpace
    char ')'
    skipSpace
    body <- expr_parser
    return . join $ aFun args body

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
    args <- sym `sepBy` (many space)
    skipSpace
    char ')'
    skipSpace
    body <- expr_parser
    return . join $ aDef name (join $ aFun args body)

toplevel_parser :: Parser (SurfaceExpr a)
toplevel_parser = do
    skipSpace
    defn <- (parens defun_parser) <|> (parens def_parser)
    skipSpace
    return defn

module_parser :: Parser [SurfaceExpr a]
module_parser = toplevel_parser `sepBy` (many space)

parse_expr
    :: (Monad m)
    => Text
    -> m (Maybe (SurfaceExpr ()))
parse_expr input = do
    let parse_result = parseOnly ((parens def_parser)
                                  <|> (parens defun_parser)
                                  <|> expr_parser) input
    case parse_result of
        Left _ -> return Nothing
        Right result -> return $ Just result

parse_multi
    :: MonadIO m
    => Text
    -> m (Maybe [SurfaceExpr ()])
parse_multi inp = do
    let the_parser = module_parser
    let parse_result = parseOnly the_parser inp
    case parse_result of
        Left reason -> do
            liftIO . putStrLn $ "Parse failure: " ++ reason
            return Nothing
        Right result -> return $ Just result

-- | Remove ";" comments from source code
removeComments :: Text -> Text
removeComments = Text.unlines . fmap fst . fmap breakComment . Text.lines
    where breakComment = Text.break (\c -> c == ';')
