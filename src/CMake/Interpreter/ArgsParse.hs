-----------------------------------------------------------------------------
-- |
-- Copyright   :  (C) 2021 AeroStun
-- License     :  Apache-2.0
--
-- Maintainer  :  AeroStun <24841307+AeroStun@users.noreply.github.com>
-- Stability   :  experimental
-- Portability :  non-portable
--
-- Command argument parsing implements
----------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module CMake.Interpreter.ArgsParse (
  ArgsParser,
  ArgumentParsingException(..),
  argsParse,
  nullaryOpt,
  unaryOpt,
  someOpt,
  unaryArg,
  manyArg,
  someArg,
  -- ByteString-specific
  parseArgs,
  integral,
  natural,
  ) where

import           CMake.AST.Defs           (SourceLocation)
import           CMake.Error              (raiseFatalError)
import           CMake.Interpreter.State  (Interp)
import           Control.Applicative      (many, some, (<|>))
import           Control.Exception        (Exception, evaluate, throw)
import           Control.Monad.Catch      (catch)
import           Control.Monad.IO.Class   (liftIO)
import           Control.Monad.State.Lazy (StateT (..), execStateT, lift,
                                           modify)
import           Data.ByteString          (ByteString)
import           Data.Data                (Typeable)
import           Data.Functor             (($>))
import           Data.Label               (set, (:->))
import qualified ParserT                  as P

type ArgsParser a b c = StateT b (P.ParserT a) c

data ArgumentParsingException a = MissingArgumentError a
                                | MissingOptionValueError a
                                | ArgumentFormatError a
                                | OptionValueFormatError a
                                | NotAnIntegerError a
                                | NotANaturalError a
                                deriving Show

instance (Show a, Typeable a) => Exception (ArgumentParsingException a)

argsParse :: Show a => [a] -> ArgsParser a b () -> b -> b
argsParse l p s = P.runParser (execStateT p s) l

nullaryOpt :: Eq a => a -> (b :-> Bool) -> ArgsParser a b ()
nullaryOpt s l = lift (P.tok s $> True <|> pure False) >>= modify . set l

unaryOpt :: (Eq a, Show a, Typeable a) => a -> (b :-> c) -> (a -> P.ParserT a c) -> ArgsParser a b ()
unaryOpt s l p = lift (P.tok s *> ((P.ensure 1 <|> throw (MissingOptionValueError s))
                             *> (p s <|> throw (OptionValueFormatError s)))) >>= modify . set l

someOpt :: (Eq a, Show a, Typeable a) => a -> (b :-> [a]) -> ArgsParser a b ()
someOpt s l = lift ((P.tok s *> (some P.item <|> throw (MissingOptionValueError s))) <|> pure []) >>= modify . set l

unaryArg :: (Show a, Typeable a) => a -> (b :-> c) -> (a -> P.ParserT a c) -> ArgsParser a b ()
unaryArg s l p = lift ((P.ensure 1 <|> throw (MissingArgumentError s)) *> (p s <|> throw (ArgumentFormatError s))) >>= modify . set l

manyArg :: (Show a, Typeable a) => a -> (b :-> [c]) -> (a -> P.ParserT a c) -> ArgsParser a b ()
manyArg s l p = lift (many (p s) <|> throw (MissingArgumentError s)) >>= modify . set l

someArg :: (Show a, Typeable a) => a -> (b :-> [c]) -> (a -> P.ParserT a c) -> ArgsParser a b ()
someArg s l p  = lift (some (p s) <|> throw (MissingArgumentError s)) >>= modify . set l


----------------------------
-- Specialised Operations --
----------------------------

parseArgs :: ByteString -> [ByteString] -> ArgsParser ByteString b () -> b -> SourceLocation -> Interp b
parseArgs f xs p b cs = handleParseEx (argsParse xs p b)
  where
    handleParseEx :: b -> Interp b
    handleParseEx a = catch (liftIO $ evaluate a) (\s -> raiseFatalError f (errMsg s) cs)
    errMsg :: ArgumentParsingException ByteString -> [ByteString]
    errMsg (MissingArgumentError field) = ["Missing argument <", field, ">"]
    errMsg (MissingOptionValueError field) = ["Missing value for option [", field, "]"]
    errMsg (ArgumentFormatError field) = ["Invalid format for argument <", field, ">"]
    errMsg (OptionValueFormatError field) = ["Invalid format for the value of option [", field, "]"]
    errMsg (NotAnIntegerError field) = [field, " is not an integer"]
    errMsg (NotANaturalError field) = [field, " is not a positive integer"]


-- | Parse and decode an Int
integral :: ByteString -> P.ParserT ByteString Int
integral s = P.integral <|> throw (NotAnIntegerError s)

-- | Parse and decode a positive Int
natural :: ByteString -> P.ParserT ByteString Int
natural s = requirePositive <$> integral s
  where
    requirePositive :: Int -> Int
    requirePositive i = if i >= 0 then i else throw (NotANaturalError s)
