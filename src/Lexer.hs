module Lexer where

import Text.Parsec
import Text.Parsec.String (Parser)
import Text.Parsec.Language (emptyDef)
import qualified Text.Parsec.Token as Tok

lexer :: Tok.TokenParser ()
lexer = Tok.makeTokenParser style
    where ops = []
          names = ["\\","=",":","let", "()"]
          idStarts = letter <|> char '_' <|> char '#'
          idLetters = letter <|> char '_' <|> digit <|> char '-'
                  <|> char '+' <|> char '?' <|> char ':' <|> char '&'
          opStarts = oneOf "!$%&|*+-/:<=>?@^_~#"
          opLetters = oneOf "!$%&|*+-/:<=>?@^_~#" <|> letter <|> digit <|> char '-' <|> char '_'
          style = emptyDef {
                      Tok.commentLine = ";"
                    , Tok.reservedOpNames = ops
                    , Tok.reservedNames = names
                    , Tok.caseSensitive = True
                    , Tok.identStart = idStarts
                    , Tok.identLetter = idLetters
                    , Tok.opStart = opStarts
                    , Tok.opLetter = opLetters
                    , Tok.commentStart = "{-"
                    , Tok.commentEnd = "-}"
          }

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

reservedOp :: String -> Parser ()
reservedOp = Tok.reservedOp lexer

identifier :: Parser String
identifier = Tok.identifier lexer

operator :: Parser String
operator = Tok.operator lexer

symbolStart :: Parser Char
symbolStart = oneOf "!$%&|*+-/:<=>?@^_~#"

symbolLetters :: Parser Char
symbolLetters = oneOf "!$%&|*+-/:<=>?@^_~#"
