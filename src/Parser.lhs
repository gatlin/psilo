---
title: "The Parser"
...

This implements the basic s-expression syntax along with some sugar, like
`let` bindings.

The result of one of the top-level parsing functions is a `Parser (Expr a)`
value from which the `Expr` value may be extracted and given to the
evaluator.

This is probably sub-optimal; parsec is a harsh master.

> module Parser where
>
> import Text.Parsec
> import Text.Parsec.String (Parser, parseFromFile)
> import Control.Applicative ((<$>))
> import Control.Monad (mapAndUnzipM)
> import Control.Monad.Free
> import Data.List.Split (splitOn)
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

Booleans are represented by the atoms `#t` and `#f`.

Symbols are like "atoms" in other lisps or Erlang. They are equivalent
only to themselves and have no intrinsic value. They are mostly used to
bind values in lambda abstractions. The symbols `#t` and `#f` are special, as
they denote Boolean truth and false values.

> parseSymbol :: Parser (Expr a)
> parseSymbol = do
>     sym <- operator <|> (optional (char '\'')) *> identifier <|> operator
>     sym' <- chomped sym
>     case sym' of
>         "#t" -> return $ Free $ ABoolean True
>         "#f" -> return $ Free $ ABoolean False
>         otherwise -> return $ Free $ ASymbol sym'
>     where chomped s = let s' = splitOn ":" s
>                       in  return $ s' !! 0

Lamba abstractions, or *functions*. A function definition is a list of symbols
to bind to the elements of the argument list, and a psilo expression to
evaluate in the context of the arguments.

> parseFn :: Parser (Expr a)
> parseFn = do
>     reserved "\\"
>     optional whitespace
>     args <- parens $ many parseSymbol
>     optional whitespace
>     body <- parseExpr
>     return $ Free $ ALambda (expr2symlist args) body

The application of a function to a list of arguments, a single symbol, or
an arbitrary expression value.

> parseApp :: Parser (Expr a)
> parseApp = do
>     fst <- ( try parseSymbol
>          <|> try parseNumber
>          <|> try (parens parseFn)
>          <|> parens parseApp )
>     optional whitespace
>     rst <- many parseExpr
>     return $ Free (AApply fst rst)

Definitions - that is, permanent additions to the environment and store - are
treated especially as strictly speaking they are not expressions.

> parseDefn :: Parser (Expr a)
> parseDefn = do
>     optional whitespace
>     reserved "="
>     optional whitespace
>     Free (ASymbol sym) <- parseSymbol
>     optional whitespace
>     body <- parseFunDef <|> parseSimpleDef
>     return $ Free $ ADefine sym body

> parseSimpleDef = parseExpr

> parseFunDef = try $ do
>     args <- parens $ many parseSymbol
>     optional whitespace
>     body <- parseExpr
>     return $ Free $ ALambda (expr2symlist args) body

Top level expression parser

> parseExpr :: Parser (Expr a)
> parseExpr = parseSymbol
>         <|> parseNumber
>         <|> parens ( parseDefn <|> parseFn <|> parseApp )

Expression parser inside a quoted list

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

> type Parsed a = Either ParseError [Expr a]

> parseFile :: String -> IO (Parsed a)
> parseFile fname = parseFromFile (contents topLevel) fname

> parseTopLevel :: String -> Parsed a
> parseTopLevel s = parse (contents topLevel) "<stdin>" s
