module Parser where

import Text.Parsec
import Text.Parsec.String (Parser, parseFromFile)
import Control.Applicative ((<$>))
import Control.Monad (mapAndUnzipM)
import Control.Monad.Free
import Data.List.Split (splitOn)

import qualified Text.Parsec.Expr as Ex
import qualified Text.Parsec.Token as Tok

import Syntax
import Lexer

parseUnit :: Parser (Expr a)
parseUnit = do
    reserved "()"
    return $ Free (AUnit)

parseNumber :: Parser (Expr a)
parseNumber = try ( do { n <- integer
                       ; return $ Free $ AInteger n
                       } )

parseSymbol :: Parser (Expr a)
parseSymbol = do
    atom <- identifier <|> operator
    return $ case atom of
        "#t"    -> Free $ ABoolean True
        "#f"    -> Free $ ABoolean False
        otherwise -> Free $ ASymbol atom

parseList :: Parser (Expr a)
parseList = fmap (Free . AList) $ sepBy parseExpr spaces

parseQuoted :: Parser (Expr a)
parseQuoted = do
    char '\''
    x <- parseExpr
    return $ Free $ AList [Free (ASymbol "quote"), x]

parseFn :: Parser (Expr a)
parseFn = do
    reserved "\\"
    optional whitespace
    args <- parens $ many parseSymbol
    optional whitespace
    body <- parseExpr
    return $ Free $ ALambda (Free (AList args)) body

parseApp :: Parser (Expr a)
parseApp = do
    fst <- ( try parseSymbol
         <|> try parseNumber
         <|> try (parens parseFn)
         <|> parens parseApp )
    optional whitespace
    rst <- many parseExpr
    return $ Free (AApply fst (Free (AList rst)))

parseDefn :: Parser (Expr a)
parseDefn = do
    optional whitespace
    reserved "="
    optional whitespace
    Free (ASymbol sym) <- parseSymbol
    optional whitespace
    body <- parseFunDef <|> parseSimpleDef
    return $ Free $ ADefine sym body

parseSimpleDef = parseExpr

parseFunDef = try $ do
    args <- parens $ many parseSymbol
    optional whitespace
    body <- parseExpr
    return $ Free $ ALambda (Free (AList args)) body

parseExpr :: Parser (Expr a)
parseExpr = parseSymbol
        <|> parseNumber
        <|> parseQuoted
        <|> parens ( parseDefn <|> parseFn <|> parseApp )

contents :: Parser a -> Parser a
contents p = do
    whitespace
    r <- p
    eof
    return r

topLevel :: Parser [Expr a]
topLevel = many $ do
    x <- parseExpr
    return x

type Parsed a = Either ParseError [Expr a]

parseFile :: String -> IO (Parsed a)
parseFile fname = parseFromFile (contents topLevel) fname

parseTopLevel :: String -> Parsed a
parseTopLevel s = parse (contents topLevel) "<stdin>" s
