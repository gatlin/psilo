{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE FlexibleContexts #-}

module Parser where

import Tubes

import Data.Functor.Identity
import Control.Applicative hiding (many, optional)
import Data.Char
import Data.Monoid
import Control.Monad (forM)
import Control.Monad.Free
import Control.Comonad
import System.IO (Handle(..), withFile, IOMode(..), hGetContents)

import Prelude hiding (map, take, tail, head, filter)

import Syntax

-- * The basic parsing framework

{- |
Our basic parser framework, built on applicatives. @s@ represents the building
blocks out of which input strings are generated. For our purposes it will
almost always be 'Char'. 't' represents the result type of the parse.
-}
newtype Parser s t = P {
    runParser :: [s] -> [(t, [s])]
}

-- | Create a parser for some result type
pPure :: t -> Parser s t
pPure x = P $ \inp -> [(x, inp)]

-- | Apply a parser to the results of another parser
pApp :: Parser s (a -> b) -> Parser s a -> Parser s b
pApp (P p1) (P p2) = P $ \inp -> do
    (v1, ss1) <- p1 inp
    (v2, ss2) <- p2 ss1
    return (v1 v2, ss2)

instance Functor (Parser s) where
    fmap f p = pApp (pPure f) p

instance Applicative (Parser s) where
    pure = pPure
    (<*>) = pApp

instance Alternative (Parser s) where
    empty = P $ \inp -> empty
    (P p1) <|> (P p2) = P $ \inp ->
        p1 inp ++ p2 inp

-- * Parsing control combinators

-- | With the supplied parser 'p' succeeds on 0 or more of its sequences
many :: Alternative f => f a -> f [a]
many p = (\x xs -> (pure x) <> xs) <$> p <*> optional (many p)

-- | With the supplied parser 'p' succeeds on 1 or more of its sequences
many1 :: Alternative f => f a -> f [a]
many1 p = (\x xs -> (pure x) <> xs) <$> p <*> many p

sepBy1 :: Alternative f => f a -> f b -> f [a]
sepBy1 p sep = (\x xs -> (pure x) <> xs) <$> p <*> many (id <$ sep <*> p)

sepBy :: Alternative f => f a -> f b -> f [a]
sepBy  p sep = sepBy1 p sep <|> (pure <$> p)

-- | Given a sequence of permissible options, succeeds if the input is any
-- of them
oneOf :: (Eq t, Foldable t1) => t1 t -> Parser t t
oneOf these = P $ \inp -> case inp of
    s:ss -> if elem s these then pure (s, ss) else empty
    _    -> empty

-- | Allows a sequence matched by 'p' to not be present in the input stream
optional :: Alternative f => f [t] -> f [t]
optional p = p <|> pure mempty

-- * Tube-specific parsing utilities

{- $tubeparsers
Using 'Tube's and 'Pump's, we are able to write parser combinators which are
agnostic to the underlying sequence type. Only the 'Pump' has to know about
lists, and soon this will not be necessary either.
-}

-- | A pump which enumerates the contents of a list
enumerator inp = pump (return inp)
            (\(Identity lst) -> case lst of
                []  -> (Nothing, Identity [])
                x:xs -> (Just x, Identity xs))
            (\(Identity xs) x -> Identity (xs ++ [x]))
{-# INLINE enumerator #-}

-- | Given a sink, constructs a parser. The actual sink need not be aware of
-- the underlying sequence type (eg, [], Text, ByteString, etc).
mkParser :: Sink (Maybe s) Identity (Maybe t) -> Parser s t
mkParser snk = P $ \inp -> runIdentity $
    runPump post (enumerator inp) snk
    where
        post :: [s] -> Maybe t -> [(t, [s])]
        post cs mc = maybe [] (\c -> [(c, cs)]) mc
        {-# INLINE post #-}

ifJust :: Monad m => Maybe a -> (a -> m (Maybe b)) -> m (Maybe b)
ifJust x what = maybe (return Nothing) what x

-- * Basic parser combinators

char :: Char -> Parser Char Char
char target = mkParser $ do
    mc <- await
    ifJust mc $ \c ->
        if target == c
            then return (Just c)
            else return Nothing

digit :: Parser Char Integer
digit = mkParser $ do
    ms <- await
    ifJust ms $ \s -> if isDigit s
        then let s' = read [s]
             in  if s' >= 0 && s' <= 9
                 then return (Just s')
                 else return Nothing
        else return Nothing

space :: Parser Char Char
space = char ' ' <|> char '\t' <|> char '\n'

whitespace :: Parser Char String
whitespace = many1 space

parens :: Parser Char a -> Parser Char a
parens p = id <$ char '(' <* optional (many space) <*> p <* optional (many space) <* char ')'

string_lit :: String -> Parser Char String
string_lit goal = mkParser $ do
    incoming <- forM [1..(length goal)] $ \_ -> do
        mc <- await
        case mc of
            Just c -> return [c]
            Nothing -> return []
    let incoming' = concat incoming
    if goal == incoming'
        then return $ Just goal
        else return Nothing

boolean :: Parser Char Bool
boolean = mkParser $ do
    blit <- forM [1,2] $ \_ -> do
        mc <- await
        case mc of
            Just c -> return [c]
            Nothing -> return []
    case (concat blit) of
        "#t"    -> return $ Just True
        "#f"    -> return $ Just False
        _       -> return Nothing

number :: Parser Char Integer
number = foldl (\a b -> a * 10 + b) 0 <$> many digit

-- * The psilo language parser

sym :: Parser Char Symbol
sym = many $ letter <|> alphanum
    <|> (oneOf "!@#$%^&*{}[]+-/")

letter :: Parser Char Char
letter = oneOf "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"

alphanum :: Parser Char Char
alphanum = letter <|> oneOf "1234567890"

aNumber x = liftF $ AInteger x
aSymbol s = liftF $ ASymbol s
aBoolean b = liftF $ ABoolean b

parse_integer :: Parser Char (Expr a)
parse_integer = aNumber <$> number

parse_symbol :: Parser Char (Expr a)
parse_symbol = aSymbol <$> sym

parse_boolean :: Parser Char (Expr a)
parse_boolean = aBoolean <$> boolean

parse_list :: Parser Char (Expr a)
parse_list = fmap Free $
    AList <$> sepBy parse_expr (many space)

parse_unit :: Parser Char (Expr a)
parse_unit = fmap Free $ AUnit <$ string_lit "'()"

parse_app :: Parser Char (Expr a)
parse_app = fmap Free $
    AApply <$> (parse_symbol <|> (parens parse_lambda))
           <*  many space
           <*> parse_list

parse_lambda :: Parser Char (Expr a)
parse_lambda = fmap Free $
    ALambda <$  char '\\'
            <*  many space
            <*> parens ( parse_list )
            <*  many space
            <*> parse_expr

parse_definition :: Parser Char (Expr a)
parse_definition = fmap Free $
    ADefine <$  char '='
            <*  many space
            <*> sym
            <*  many space
            <*> parse_expr

parse_expr :: Parser Char (Expr a)
parse_expr =
        (parens parse_lambda)
    <|> (parens parse_app)
    <|> (parens parse_definition)
    <|> parse_integer
    <|> parse_boolean
    <|> parse_unit
    <|> parse_symbol

drain :: Monad m
      => Source b m ()
      -> m (Pump (Maybe b) b Identity [b])
drain src = reduce send (enumerator []) id src

parse :: (Monad m)
      => String
      -> Source (Expr (), String) m ()
parse expr = each (runParser parse_expr expr)

parseTopLevel :: String -> Either String [Expr ()]
parseTopLevel inp = do
    en <- drain (parse inp >< map fst >< take 1)
    case extract en of
        []  -> Left "Parse failed idk y"
        (x:_) -> Right [x]

parseFile :: String -> IO (Either String [Expr ()])
parseFile fn = withFile fn ReadMode $ \hndl -> do
    contents <- hGetContents hndl
    let loop ""  []     = return []
        loop txt exprs = do
            e <- drain (parse txt >< take 1)
            let expr = extract e
            case expr of
                []  -> return exprs
                ((expr', txt'):_) -> loop txt' (expr':exprs)
    result <- loop contents []
    case result of
        []  -> return $ Left "fucked"
        _   -> return $ Right result
