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
    args <- parens parseList
    optional whitespace
    body <- parseExpr
    let name' = case name of
                    Nothing -> Anonymous
                    Just n  -> Named n
    return $ Free $ ALambda name' args body

parseApp :: Parser (PExpr a)
parseApp = do
    fun <- parseExpr
    optional whitespace
    body <- parseExpr <|> return (Free (AList []))
    case body of
        (Free (AList _)) -> return $ Free (fun :. body)
        _                -> let body' = (Free . AList) [body]
                            in  return $ Free (fun :. body')

parseList :: Parser (PExpr a)
parseList = fmap (Free . AList) $ parseExprInQuote `sepBy` whitespace

parseUnquotable :: Parser (PExpr a)
parseUnquotable = fmap (Free . AList) $ parseExprInQuasi `sepBy` whitespace

parseQuote :: Parser (PExpr a)
parseQuote = do
    x <- parseSymbol <|> parseNumber <|> parseList
    return $ (Free . AList) [(Free . ASymbol) "quote", x]

parseQuasi :: Parser (PExpr a)
parseQuasi = do
    x <- parseSymbol <|> parseNumber <|> parseUnquotable
    return $ (Free . AList) [(Free . ASymbol) "quote", x]

parseLet :: Parser (PExpr a)
parseLet = do
    reserved "let"
    optional whitespace
    (Free (AList assns)) <- parens parseList
    body  <- parseExpr <|> return (Free (AList []))
    (args,operands) <- (flip mapAndUnzipM) assns $ \(Free (AList (x:y:_))) -> return (x,y)
    args' <- return $ Free $ AList args
    operands' <- return $ Free $ AList operands
    fun <- return $ Free $ ALambda Anonymous args' body
    return $ Free (fun :. operands')

parseExpr :: Parser (PExpr a)
parseExpr = parseSymbol
        <|> parseNumber
        <|> (try (char '\'') >> parseQuote)
        <|> (try (char '`')  >> parseQuasi)
        <|> parens ( parseFn <|> parseLet <|> parseApp )

-- | Essentially removes the ability to evaluate any terms
parseExprInQuote :: Parser (PExpr a)
parseExprInQuote = parseSymbol
               <|> parseNumber
               <|> (try (char '\'') >> parseQuote)
               <|> parens ( parseList )

-- | Begin a list but allow for the unquote operator
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
