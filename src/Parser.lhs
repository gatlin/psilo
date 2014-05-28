Parser
===

This implements the basic s-expression syntax along with some sugar, like
`let` bindings.

The result of one of the top-level parsing functions is a `Parser (Expr a)`
value from which the `Expr` value may be extracted and given to the
evaluator.

This is probably sub-optimal; parsec is a harsh master.

> module Parser where
>
> import Text.Parsec
> import Text.Parsec.String (Parser)
> import Control.Applicative ((<$>))
> import Control.Monad (mapAndUnzipM)
> import Control.Monad.Free
>
> import qualified Text.Parsec.Expr as Ex
> import qualified Text.Parsec.Token as Tok
>
> import Syntax
> import Lexer
>
> parseNumber :: Parser (Expr a)
> parseNumber = try ( do { n <- integer
>                        ; return $ Free $ AInteger n
>                        } )
>

Symbols are like "atoms" in other lisps or Erlang. They are equivalent
only to themselves and have no intrinsic value. They are mostly used to
bind values in lambda abstractions.

> parseSymbol :: Parser (Expr a)
> parseSymbol = do
>     sym <- identifier
>     return $ Free $ ASymbol sym
>

Lambda abstractions. Optional name. Accepts a list of symbols to bound and
an expression to evaluate inside the newly created scope.

> parseFn :: Parser (Expr a)
> parseFn = do
>     reserved "\\"
>     optional whitespace
>     (Free (ASymbol arg)) <- parens parseSymbol
>     optional whitespace
>     body <- parseExpr
>     return $ Free $ ALambda arg body
>
> parseLetBinding :: Parser (Expr a)
> parseLetBinding = do
>     optional whitespace
>     sym <- parseSymbol
>     optional whitespace
>     val <- parseExpr
>     return $ Free . AList $ [sym, val]
>
> parseLetBindings :: Parser (Expr a)
> parseLetBindings = fmap (Free . AList) $ parens parseLetBinding `sepBy` whitespace
>

This translates into the application of an anonymous function to a list of
arguments.

> parseLet :: Parser (Expr a)
> parseLet = do
>     reserved "let"
>     optional whitespace
>     (Free (AList assns)) <- parens parseLetBindings
>     body  <- parseExpr <|> return (Free (AList []))
>     (args,operands) <- (flip mapAndUnzipM) assns $ \(Free (AList (x:y:_))) -> return (x,y)
>     --args' <- return $ Free $ AList args
>     operands' <- return $ Free $ AList operands
>     (Free (ASymbol arg1)) <- return $ args !! 0
>     fun <- return $ Free $ ALambda arg1 body
>     return $ Free (AApply fun operands')
>

The application of a function to a list of arguments, a single symbol, or
an arbitrary expression value.

> parseApp :: Parser (Expr a)
> parseApp = do
>     try (reserved "apply") >> (do
>         optional whitespace
>         fun <- parseExpr
>         optional whitespace
>         body <- (try (char '\'') >> parens parseQuotedList)
>             <|> (try (char '`') >> parens parseUnquotable)
>         return $ Free (AApply fun body))
>     <|> (do
>         fst <- try parseSymbol <|> try (parens parseFn) <|> parens parseApp
>         optional whitespace
>         rst <- fmap (Free . AList) $ parseExpr `sepBy` whitespace
>         return $ Free (AApply fst rst))

Regular list created by the quote (') operator. Enters a state where
everything is treated as a literal - no applications allowed.

> parseQuotedList :: Parser (Expr a)
> parseQuotedList = fmap (Free . AList) $ parseExprInQuote `sepBy` whitespace

Similar to a quoted list, except a comma operator (,) may be used to go
back into a state where application is allowed.

> parseUnquotable :: Parser (Expr a)
> parseUnquotable = fmap (Free . AList) $ parseExprInQuasi `sepBy` whitespace

Many things may be quoted, not just lists.

> parseQuote :: Parser (Expr a)
> parseQuote = do
>     x <- parseSymbol <|> parseNumber <|> parens parseQuotedList
>     return $ (Free . AList) [(Free . ASymbol) "quote", x]

You can quasi-quote anything you can quote, though this is of dubious
utility.

> parseQuasi :: Parser (Expr a)
> parseQuasi = do
>     x <- parseSymbol <|> parseNumber <|> parens parseUnquotable
>     return $ (Free . AList) [(Free . ASymbol) "quasi", x]

> parseComma :: Parser (Expr a)
> parseComma = do
>     x <- parseSymbol <|> parseExpr
>     return $ (Free . AList) [(Free . ASymbol) "comma", x]

> parseAdd :: Parser (Expr a)
> parseAdd = do
>     reservedOp "+"
>     args <- parseQuotedList
>     return $ Free $ AAdd args

> parseMult :: Parser (Expr a)
> parseMult = do
>     reservedOp "*"
>     args <- parseQuotedList
>     return $ Free $ AMult args

Top level expression parser

> parseExpr :: Parser (Expr a)
> parseExpr = parseSymbol
>         <|> parseNumber
>         <|> (try (char '\'') >> parseQuote)
>         <|> (try (char '`')  >> parseQuasi)
>         <|> parens ( parseMult <|> parseAdd <|> parseFn <|> parseLet <|> parseApp )

Expression parser inside a quoted list

> parseExprInQuote :: Parser (Expr a)
> parseExprInQuote = parseSymbol
>                <|> parseNumber
>                <|> (try (char '\'') >> parseQuote)
>                <|> parens ( parseMult <|> parseAdd <|> parseQuotedList )

Expression parser inside a quasiquoted list

> parseExprInQuasi :: Parser (Expr a)
> parseExprInQuasi = parseSymbol
>                <|> parseNumber
>                <|> (try (reserved "'") >>  parseQuote)
>                <|> (try (reserved "`" ) >> parseQuasi)
>                <|> (try (char ',')  >>     parseComma)
>                <|> parens ( parseUnquotable )

> contents :: Parser a -> Parser a
> contents p = do
>     whitespace
>     r <- p
>     eof
>     return r

> topLevel :: Parser [Expr a]
> topLevel = many $ do
>     x <- parseExpr
>     return x

> doParse :: String -> Either ParseError (Expr a)
> doParse s = parse (contents parseExpr) "<stdin>" s

> parseTopLevel :: String -> Either ParseError [Expr a]
> parseTopLevel s = parse (contents topLevel) "<stdin>" s
