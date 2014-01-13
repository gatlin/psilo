module Lexer where

import Text.Parsec
import Text.Parsec.String (Parser)
import Text.Parsec.Language (emptyDef)
import qualified Text.Parsec.Token as Tok

lexer :: Tok.TokenParser ()
lexer = Tok.makeTokenParser style
    where ops = ["+","*","-","/","<","="]
          names = ["fn","::"]
          style = emptyDef {
                      Tok.commentLine = ";"
                    , Tok.reservedOpNames = ops
                    , Tok.reservedNames = names
                    , Tok.caseSensitive = True
          }

float :: Parser Double
float = Tok.float lexer

integer :: Parser Integer
integer = Tok.integer lexer

parens :: Parser a -> Parser a
parens = Tok.parens lexer

whitespace :: Parser ()
whitespace = Tok.whiteSpace lexer

nl :: Parser ()
nl = skipMany newline

reserved :: String -> Parser ()
reserved = Tok.reserved lexer

symbol :: Parser Char
symbol = oneOf "!$%&|*+-/:<=>?@^_~#"

reservedOp :: String -> Parser ()
reservedOp = Tok.reservedOp lexer

identifier :: Parser String
identifier = Tok.identifier lexer
