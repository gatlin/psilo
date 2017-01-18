{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ApplicativeDo #-}

module Lib.Parser where

import Prelude hiding (map, take, filter)
import qualified Prelude as P
import Control.Monad (forever, forM, forM_)
import Control.Monad.Trans
import Control.Applicative hiding (many, optional)
import Data.Monoid ((<>))
import Data.Char (isDigit, isPrint)
import Control.Monad.Free
import Data.Bool
import Control.Arrow ((***))
import Data.Either (isRight)
import Data.List (intercalate, replicate, null)

import Tubes
import Lib.Syntax

-- |
-- A 'Parser' transforms a stream of tokens into a stream
-- of possible parses along with unused input.
newtype Parser m s t = P {
    runParser :: Source m s -> Source m (t, Source m s)
    }

instance Monad m => Functor (Parser m s) where
    fmap f (P srcFn) = P $ \src ->
                               fmap (\(a, b) ->
                                         (f a, b)) $ srcFn src

instance Monad m => Applicative (Parser m s) where
    pure x = P $ \inp -> Source $ yield (x, inp)
    (P p1) <*> (P p2) = P $ \inp -> do
        (v1, ss1) <- p1 inp
        (v2, ss2) <- p2 ss1
        return (v1 v2, ss2)

instance Monad m => Alternative (Parser m s) where
    empty = P $ \inp -> empty
    (P p1) <|> (P p2) = P $ \inp -> (p1 inp) <|> (p2 inp)

-- * Fundamental parser combinators

(<++>) :: Applicative f => f [a] -> f [a] -> f [a]
a <++> b = (++) <$> a <*> b

(<:>) :: Applicative f => f a -> f [a] -> f [a]
a <:> b = (:) <$> a <*> b

optional :: (Alternative f, Monoid a) => f a -> f a
optional p = p <|> pure mempty

many :: (Alternative f, Monoid (g a), Applicative g) => f a -> f (g a)
many p = (\x xs -> (pure x) <> xs) <$> p <*> optional (many p)

sepBy1 :: (Alternative f, Monoid (g a), Applicative g)
       => f a -> f b -> f (g a)
sepBy1 p sep = (\x xs -> (pure x) <> xs) <$> p <*> many (id <$ sep <*> p)

sepBy :: (Alternative f, Monoid (g a), Applicative g)
      => f a -> f b -> f (g a)
sepBy p sep = sepBy1 p sep <|> (pure <$> p)

-- * More advanced parser combinators, and 'Char' based parsers

-- | Succeeds iff the incoming token is among those in the argument list.
oneOf :: (Monad m, Eq t) => [t] -> Parser m t t
oneOf these = P $ \inp -> Source $ do
    mv <- lift $ unyield $ sample inp
    case mv of
        Nothing -> halt
        Just (c, inp') -> if elem c these
            then yield (c, Source inp')
            else halt

-- | Succeeds iff the next token is equal to the specified character.
char :: Monad m => Char -> Parser m Char Char
char target = P $ \inp -> Source $ do
    mC <- lift $ unyield $ sample inp
    case mC of
        Nothing -> halt
        Just (c, inp') ->
            if c == target
                then yield (c, Source inp')
                else halt

-- | Succeeds iff the incoming token is a printable character.
printable :: Monad m => Parser m Char Char
printable = P $ \inp -> Source $ do
    mc <- lift $ unyield $ sample inp
    case mc of
        Nothing -> halt
        Just (c, inp') ->
            if isPrint c
                then yield (c, Source inp')
                else halt

-- | Succeeds if the incoming token is a numeric digit, and parses it as such.
digit :: Monad m=> Parser m Char Int
digit = P $ \inp -> Source $ do
    ms <- lift $ unyield $ sample inp
    case ms of
        Nothing -> halt
        Just (c, inp') -> if isDigit c
            then let d = read [c]
                 in  if d >= 0 && d <= 9
                         then yield (d, Source inp')
                         else halt
            else halt

-- | Succeeds iff the next tokens form the given string.
string :: Monad m => String -> Parser m Char String
string target = P $ \inp -> Source $ go target [] (sample inp) where
    go [] acc inp = yield (acc, Source inp)
    go (x:xs) acc inp = do
        mc <- lift $ unyield inp
        case mc of
            Nothing -> halt
            Just (c, inp') ->
                if c == x
                    then go xs (acc++[c]) inp'
                    else halt

boolean :: Monad m => Parser m Char Bool
boolean = fmap (\case
                       "#t" -> True
                       "#f" -> False) $ (string "#t") <|> (string "#f")

-- * Programming language parsing utilities for 'Char' streams

num :: (Monad m) => Parser m Char String
num = many (oneOf "1234567890")

plus :: (Monad m) => Parser m Char String
plus = char '+' *> num

minus :: (Monad m) => Parser m Char String
minus = char '-' *> num

-- | Succeeds iff the tokens form an integer literal
integer :: Monad m => Parser m Char Int
integer = rd <$> (plus <|> minus <|> num)
    where rd = read :: String -> Int

-- | Succeeds iff the tokens form a double literal
double :: Monad m => Parser m Char Double
double = fmap rd $ num' <++> decimal <++> exponent where
    rd = read :: String -> Double
    num' = plus <|> minus <|> num
    decimal = optional $ char '.' <:> num
    exponent = optional $ oneOf "eE" <:> num

-- | Succeeds iff the token is a single whitespace character
space :: Monad m => Parser m Char Char
space = char ' ' <|> char '\t' <|> char '\n'

spaces :: Monad m => Parser m Char String
spaces = many space

-- | Succeeds iff the given sub-parser suceeds and is delimited by the
-- specified tokens.
delim :: Monad m => Char -> Char -> Parser m Char a -> Parser m Char a
delim start end p = id
    <$  char start
    <*  optional spaces
    <*> p
    <*  optional spaces
    <*  char end

-- | 'delim' specifically for parentheses
parens :: Monad m => Parser m Char a -> Parser m Char a
parens = delim '(' ')'

-- | 'delim' specifically for double quotes
quotes :: Monad m => Parser m Char a -> Parser m Char a
quotes = delim '"' '"'

-- | Succeeds iff the next tokens form a legal symbol
sym :: Monad m => Parser m Char String
sym = many $ letter <|> alphanum <|> (oneOf "=<>.!@#$%^&*{}[]+-/\\")

letter :: Monad m => Parser m Char Char
letter = oneOf "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"

alphanum :: Monad m => Parser m Char Char
alphanum = letter <|> oneOf "1234567890"

-- * Parser combinators

parse_num :: Monad m => Parser m Char (Expr a)
parse_num = aNumber <$> double

parse_bool :: Monad m => Parser m Char (Expr a)
parse_bool = aBool <$> boolean

parse_id :: Monad m => Parser m Char (Expr a)
parse_id = aId <$> sym

parse_string :: Monad m => Parser m Char (Expr a)
parse_string = aString <$> (quotes (many printable))

parse_if :: Monad m => Parser m Char (Expr a)
parse_if = fmap Free $ IfC
    <$  string "if"
    <*  optional spaces
    <*> parse_expr
    <*  optional spaces
    <*> parse_expr
    <*  optional spaces
    <*> parse_expr

parse_app :: Monad m => Parser m Char (Expr a)
parse_app = fmap Free $ AppC
    <$> (parse_id <|> parse_expr)
    <*  optional spaces
    <*> (parse_expr `sepBy` spaces)

parse_clos :: Monad m => Parser m Char (Expr a)
parse_clos = fmap Free $ ClosC
    <$  (string "lambda" <|> string "\\")
    <*  optional spaces
    <*> parens (sym `sepBy` spaces)
    <*  optional spaces
    <*> parse_expr

parse_expr :: Monad m => Parser m Char (Expr a)
parse_expr = parse_bool
         <|> parse_num
         <|> parse_id
         <|> parse_string
         <|> parens parse_if
         <|> parens parse_clos
         <|> parens parse_app

parse :: (Foldable f, Monad m)
      => f Char
      -> Source m (Expr a, Source m Char)
parse src = runParser parse_expr (Source (each src))
