module Parser where

import Text.Parsec
import Text.Parsec.String (Parser)
import Control.Applicative ((<$>))
import Control.Monad.Free

import qualified Text.Parsec.Expr as Ex
import qualified Text.Parsec.Token as Tok

import Syntax
import Lexer

parseNumber :: Parser (PExpr a)
parseNumber = try ( do { f <- float
                       ; return $ Free $ AFloat f
                       } )
          <|> try ( do { n <- integer
                       ; return $ Free $ AInteger n
                       } )

parseSymbol :: Parser (PExpr a)
parseSymbol = do
    let syms = oneOf "!$%&|*+-/:<=>?@^_~#"
    first <- letter <|> syms
    rest  <- many $ letter <|> digit <|> syms
    let sym = [first] ++ rest
    return $ case sym of
               "#t"      -> Free $ ABoolean True
               "#f"      -> Free $ ABoolean False
               otherwise -> Free $ ASymbol sym

parseFn :: Parser (PExpr a)
parseFn = do
    reserved "fn"
    whitespace
    arg <- identifier
    whitespace
    body <- parseExpr
    return $ Free $ ALambda arg body

parseApp :: Parser (PExpr a)
parseApp = do
    fun <- parseExpr
    whitespace
    body <- parseExpr
    return $ Free ( fun :. body )

parseExpr :: Parser (PExpr a)
parseExpr = parseSymbol
        <|> parseNumber
        <|> parens ( parseFn <|> parseApp )

contents :: Parser a -> Parser a
contents p = do
    whitespace <|> nl
    r <- p
    eof
    return r

topLevel :: Parser [PExpr a]
topLevel = many $ do
    x <- parseExpr
    return x

doParse :: String -> Either ParseError (PExpr a)
doParse s = parse (contents parseExpr) "<stdin>" s

parseTopLevel :: String -> Either ParseError [PExpr a]
parseTopLevel s = parse (contents topLevel) "<stdin>" s
