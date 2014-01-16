module Parser where

import Text.Parsec
import Text.Parsec.String (Parser)
import Control.Applicative ((<$>))
import Control.Monad (mapAndUnzipM)
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
    name <- optionMaybe identifier
    args <- parens parseQuotedList
    optional whitespace
    body <- parseExpr
    let name' = case name of
                    Nothing -> Anonymous
                    Just n  -> Named n
    return $ Free $ ALambda name' args body

parseApp :: Parser (PExpr a)
parseApp = do
    reserved "apply"
    optional whitespace
    fun <- parseExpr
    optional whitespace
    body <- (try (char '\'') >> parens parseQuotedList)
        <|> (try (char '`') >> parens parseUnquotable)
        <|> parseExpr
    return $ Free (AApply fun body)

parseQuotedList :: Parser (PExpr a)
parseQuotedList = fmap (Free . AList) $ parseExprInQuote `sepBy` whitespace

parseUnquotable :: Parser (PExpr a)
parseUnquotable = fmap (Free . AList) $ parseExprInQuasi `sepBy` whitespace

parseQuote :: Parser (PExpr a)
parseQuote = do
    x <- parseSymbol <|> parseNumber <|> parseQuotedList
    return $ (Free . AList) [(Free . ASymbol) "quote", x]

parseQuasi :: Parser (PExpr a)
parseQuasi = do
    x <- parseSymbol <|> parseNumber <|> parseUnquotable
    return $ (Free . AList) [(Free . ASymbol) "quote", x]

parseLet :: Parser (PExpr a)
parseLet = do
    reserved "let"
    optional whitespace
    (Free (AList assns)) <- parens parseUnquotable
    body  <- parseExpr <|> return (Free (AList []))
    (args,operands) <- (flip mapAndUnzipM) assns $ \(Free (AList (x:y:_))) -> return (x,y)
    args' <- return $ Free $ AList args
    operands' <- return $ Free $ AList operands
    fun <- return $ Free $ ALambda Anonymous args' body
    return $ Free (AApply fun operands')

-- | Top level expression parser
parseExpr :: Parser (PExpr a)
parseExpr = parseSymbol
        <|> parseNumber
        <|> (try (char '\'') >> parseQuote)
        <|> (try (char '`')  >> parseQuasi)
        <|> parens ( parseFn <|> parseLet <|> parseApp )

-- | Expression parser inside a quoted list
parseExprInQuote :: Parser (PExpr a)
parseExprInQuote = parseSymbol
               <|> parseNumber
               <|> (try (char '\'') >> parseQuote)
               <|> parens ( parseQuotedList )

-- | Expression parser inside a quasiquoted list
parseExprInQuasi :: Parser (PExpr a)
parseExprInQuasi = parseSymbol
               <|> parseNumber
               <|> (try (reserved "\'") >> parseQuasi)
               <|> (try (reserved ",")  >> parseExpr)
               <|> parens ( parseUnquotable )

contents :: Parser a -> Parser a
contents p = do
    whitespace
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
