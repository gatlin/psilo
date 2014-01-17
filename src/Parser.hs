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

-- | Symbols are like "atoms" in other lisps or Erlang. They are equivalent
-- only to themselves and have no intrinsic value. They are mostly used to
-- bind values in lambda abstractions.
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

-- | Lambda abstractions. Optional name. Accepts a list of symbols to bound and
-- an expression to evaluate inside the newly created scope.
parseFn :: Parser (PExpr a)
parseFn = do
    reserved "fn"
    optional whitespace
    name <- optionMaybe identifier
    args <- parens parseSymbolList
    optional whitespace
    body <- parseExpr
    let name' = case name of
                    Nothing -> Anonymous
                    Just n  -> Named n
    return $ Free $ ALambda name' args body

-- | The application of a function to a list of arguments, a single symbol, or
-- an arbitrary expression value.
parseApp :: Parser (PExpr a)
parseApp = do
    try (reserved "apply") >> (do
        optional whitespace
        fun <- parseExpr
        optional whitespace
        body <- (try (char '\'') >> parens parseQuotedList)
            <|> (try (char '`') >> parens parseUnquotable)
        return $ Free (AApply fun body))
    <|> (do
        fst <- parseSymbol <|> parens parseFn
        optional whitespace
        rst <- fmap (Free . AList) $ parseExpr `sepBy` whitespace
        return $ Free (AApply fst rst))


-- | Special restricted list for lambda argument lists. Can only contain
-- symbols.
parseSymbolList :: Parser (PExpr a)
parseSymbolList = fmap (Free . AList) $ (parseSymbol <|> parens parseSymbolList) `sepBy` whitespace

-- | Regular list created by the quote (') operator. Enters a state where
-- everything is treated as a literal - no applications allowed.
parseQuotedList :: Parser (PExpr a)
parseQuotedList = fmap (Free . AList) $ parseExprInQuote `sepBy` whitespace

-- | Similar to a quoted list, except a comma operator (,) may be used to go
-- back into a state where application is allowed.
parseUnquotable :: Parser (PExpr a)
parseUnquotable = fmap (Free . AList) $ parseExprInQuasi `sepBy` whitespace

-- | Many things may be quoted, not just lists.
parseQuote :: Parser (PExpr a)
parseQuote = do
    x <- parseSymbol <|> parseNumber <|> parens parseQuotedList
    return $ (Free . AList) [(Free . ASymbol) "quote", x]

-- | You can quasi-quote anything you can quote, though this is of dubious
-- utility.
parseQuasi :: Parser (PExpr a)
parseQuasi = do
    x <- parseSymbol <|> parseNumber <|> parens parseUnquotable
    return $ (Free . AList) [(Free . ASymbol) "quasi", x]

parseComma :: Parser (PExpr a)
parseComma = do
    x <- parseSymbol <|> parseExpr
    return $ (Free . AList) [(Free . ASymbol) "comma", x]

parseLetBinding :: Parser (PExpr a)
parseLetBinding = do
    optional whitespace
    sym <- parseSymbol
    optional whitespace
    val <- parseExpr
    return $ Free . AList $ [sym, val]

parseLetBindings :: Parser (PExpr a)
parseLetBindings = fmap (Free . AList) $ parens parseLetBinding `sepBy` whitespace

-- | This translates into the application of an anonymous function to a list of
-- arguments.
parseLet :: Parser (PExpr a)
parseLet = do
    reserved "let"
    optional whitespace
    (Free (AList assns)) <- parens parseLetBindings
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
               <|> (try (reserved "'") >>  parseQuote)
               <|> (try (reserved "`" ) >> parseQuasi)
               <|> (try (char ',')  >>     parseComma)
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
