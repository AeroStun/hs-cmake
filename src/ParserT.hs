-----------------------------------------------------------------------------
-- |
-- Copyright   :  (C) 2021 AeroStun
-- License     :  Apache-2.0
--
-- Maintainer  :  AeroStun <24841307+AeroStun@users.noreply.github.com>
-- Stability   :  experimental
-- Portability :  non-portable
--
-- Derived from works under MIT Copyright (c) 2014-2016, Stephen Diehl
-- https://github.com/sdiehl/write-you-a-haskell/blob/master/chapter3/parsec.hs
--
-- Simple parser-combinator over a list of arbitrary token type
----------------------------------------------------------------------------
{-# LANGUAGE LambdaCase #-}

module ParserT (
  ParserT,
  item,
  eof,
  ensure,
  satisfy,
  tok,
  notTok,
  toks,
  oneOf,
  chainl,
  chainl1,
  parseList,
  runParser
  ) where
import           Control.Applicative (Alternative (..))
import           Control.Monad       (MonadPlus (..), void)


-- | Simple parser for a given token-type and result-type
newtype ParserT t r = ParserT { parse :: [t] -> [(r, [t])] }

item :: ParserT t t
item = ParserT $ \case
                   []       -> []
                   (c : cs) -> [(c, cs)]

eof :: ParserT t ()
eof = ParserT $ \case
              [] -> [((), [])]
              _  -> []

-- | Succeeds only if at least n tokens are available
ensure :: Int -> ParserT t ()
ensure n = ParserT $ \l -> [((), l) | n == length (take n $ void l)]

bind :: ParserT t a -> (a -> ParserT t b) -> ParserT t b
bind p f = ParserT $ \s -> concatMap (\(a, s') -> parse (f a) s') $ parse p s

unit :: a -> ParserT t a
unit a = ParserT (\s -> [(a,s)])

instance Functor (ParserT t) where
  fmap f (ParserT cs) = ParserT (\s -> [(f a, b) | (a, b) <- cs s])

instance Applicative (ParserT t) where
  pure = return
  (ParserT cs1) <*> (ParserT cs2) = ParserT (\s -> [(f a, s2) | (f, s1) <- cs1 s, (a, s2) <- cs2 s1])

instance Monad (ParserT t) where
  return = unit
  (>>=)  = bind

instance MonadPlus (ParserT t) where
  mzero = failure
  mplus = combine

instance Alternative (ParserT t) where
  empty = mzero
  (<|>) = option

combine :: ParserT t a -> ParserT t a -> ParserT t a
combine p q = ParserT (\s -> parse p s ++ parse q s)

failure :: ParserT t a
failure = ParserT $ const []
{-# INLINE failure #-}

option :: ParserT t a -> ParserT t a -> ParserT t a
option  p q = ParserT $ \s ->
  case parse p s of
    []  -> parse q s
    res -> res

satisfy :: (t -> Bool) -> ParserT t t
satisfy p = item `bind` \c ->
  if p c
  then unit c
  else failure


tok :: Eq t => t -> ParserT t t
tok c = satisfy (c ==)
{-# INLINE tok #-}

notTok :: Eq t => t -> ParserT t t
notTok c = satisfy (c /=)
{-# INLINE notTok #-}

toks :: Eq t => [t] -> ParserT t [t]
toks = traverse tok

oneOf :: Eq t => [t] -> ParserT t t
oneOf s = satisfy (`elem` s)

chainl :: ParserT t a -> ParserT t (a -> a -> a) -> a -> ParserT t a
chainl p op a = (p `chainl1` op) <|> return a

chainl1 :: ParserT t a -> ParserT t (a -> a -> a) -> ParserT t a
p `chainl1` op = do {a <- p; rest a}
  where rest a = (do f <- op
                     b <- p
                     rest (f a b))
                 <|> return a

parseList :: ParserT t r -> [t] -> Maybe r
parseList m s =
  case parse m s of
    [(res, [])] -> Just res
    _           -> Nothing

runParser :: Show t => ParserT t r -> [t] -> r
runParser m s =
  case parse m s of
    [(res, [])] -> res
    [(_, rest)] -> error $ "Parser did not consume entire stream. Left " ++ show rest
    _           -> error "Parser error."
