module Parser where

import Text.Parsec
import Text.Parsec.String (Parser)
import Control.Applicative ((<$>))

import qualified Text.Parsec.Expr as Ex
import qualified Text.Parsec.Token as Tok

import Syntax
import Lexer

parseFloat :: Parser PExpr
parseFloat = do
    n <- float
    return $ Float n

parseInteger :: Parser PExpr
parseInteger = fmap (Integer . read) $ many1 digit

parseNumber :: Parser PExpr
parseNumber = try ( do { f <- float
                       ; return $ Float f
                       } )
          <|> try ( do { n <- integer
                       ; return $ Integer n
                       } )

parseSymbol :: Parser PExpr
parseSymbol = do
    first <- letter <|> symbol
    rest  <- many $ letter <|> digit <|> symbol
    let sym = [first] ++ rest
    return $ case sym of
               "#t"      -> Boolean True
               "#f"      -> Boolean False
               otherwise -> Symbol sym

parseList :: Parser PExpr
parseList = fmap List $ parseExpr `sepBy` (whitespace <|> nl)

parseQuoted :: Parser PExpr
parseQuoted = do
    char '\''
    x <- parseExpr
    return $ List [Symbol "quote", x]

parseExpr :: Parser PExpr
parseExpr = parseSymbol
        <|> parseNumber
        <|> parseQuoted
        <|> parens parseList

contents :: Parser a -> Parser a
contents p = do
    whitespace <|> nl
    r <- p
    eof
    return r

topLevel :: Parser [PExpr]
topLevel = many $ do
    x <- parseExpr
    return x

doParse :: String -> Either ParseError PExpr
doParse s = parse (contents parseExpr) "<stdin>" s

parseTopLevel :: String -> Either ParseError [PExpr]
parseTopLevel s = parse (contents topLevel) "<stdin>" s
