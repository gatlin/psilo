module Lexer where

import Text.Parsec
import Text.Parsec.String (Parser)
import Text.Parsec.Language (emptyDef)
import qualified Text.Parsec.Token as Tok

lexer :: Tok.TokenParser ()
lexer = Tok.makeTokenParser style
    where ops = ["+","*","-","/","<","="]
          names = ["fn","::","let","apply"]
          idStarts = letter <|> char '_'
          idLetters = letter <|> char '_' <|> digit <|> char '-' <|> char '+' <|> char '?'
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
                    , Tok.commentStart = "/*"
                    , Tok.commentEnd = "*/"
          }

float :: Parser Double
float = Tok.float lexer

integer :: Parser Integer
integer = Tok.integer lexer

parens :: Parser a -> Parser a
parens = Tok.parens lexer

brackets :: Parser a -> Parser a
brackets = Tok.brackets lexer

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
