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
    optional whitespace
    args <- parens parseList
    optional whitespace
    body <- parseExpr
    return $ Free $ ALambda args body

parseApp :: Parser (PExpr a)
parseApp = do
    fun <- parseExpr
    optional whitespace
    body <- parseExpr
    return $ Free ( fun :. body )

parseList :: Parser (PExpr a)
parseList = fmap (Free . AList) $ parseExprInQuote `sepBy` whitespace

parseUnquotable :: Parser (PExpr a)
parseUnquotable = fmap (Free . AList) $ parseExprInQuasi `sepBy` whitespace

parseQuote :: Parser (PExpr a)
parseQuote = do
    x <- parseExprInQuote
    return $ (Free . AList) [(Free . ASymbol) "quote", x]

parseQuasi :: Parser (PExpr a)
parseQuasi = do
    x <- parseExprInQuasi
    return $ (Free . AList) [(Free . ASymbol) "quote", x]

parseExpr :: Parser (PExpr a)
parseExpr = parseSymbol
        <|> parseNumber
        <|> (try (reserved "\'") >> parseQuote)
        <|> (try (reserved "`")  >> parseQuasi)
        <|> parens ( parseFn <|> parseApp )

-- | Essentially removes the ability to evaluate any terms
parseExprInQuote :: Parser (PExpr a)
parseExprInQuote = parseSymbol
               <|> parseNumber
               <|> (try (reserved "\'") >> parseQuote)
               <|> parens ( parseFn <|> parseList)

-- | Begin a list but allow for the unquote operator
parseExprInQuasi :: Parser (PExpr a)
parseExprInQuasi = parseSymbol
               <|> parseNumber
               <|> (try (reserved "\'") >> parseQuasi)
               <|> (try (reserved ",")  >> parseExpr)
               <|> parens ( parseFn <|> parseUnquotable )

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
