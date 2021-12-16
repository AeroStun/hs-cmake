-----------------------------------------------------------------------------
-- |
-- Copyright   :  (C) 2021 AeroStun
-- License     :  Apache-2.0
--
-- Maintainer  :  AeroStun <24841307+AeroStun@users.noreply.github.com>
-- Stability   :  experimental
-- Portability :  non-portable
--
-- Parsing rules for CMake condition syntax
-- See https://cmake.org/cmake/help/latest/command/if.html
----------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
module CMake.Cond.Parser (
  Cond(..),
  UnaryOp(..),
  BinaryOp(..),
  condition
  ) where
import           CMake.Cond.Defs         (BinaryOp (..), Cond (..),
                                          UnaryOp (..), falsy, truthy)
import           Control.Applicative     ((<|>))
import           Data.ByteString         (ByteString)
import           Data.Functor            (($>))
import           ParserT                 (ParserT, chainl1, item, satisfy, tok)
import           Text.Parser.Combinators (between)

type ArgParser a = ParserT ByteString a

-- | Map token to a constant
(-$>) :: Eq t => t -> r -> ParserT t r
tk -$> c = tok tk $> c

-- | CMake constant
constant :: ArgParser Cond
constant = Constant <$> ((satisfy truthy $> True) <|> (satisfy falsy $> False))

infixOp :: ByteString -> (Cond -> Cond -> Cond) -> ArgParser (Cond -> Cond -> Cond)
infixOp x f = tok x >> return f

prefixOp :: ByteString -> (Cond -> Cond) -> ArgParser Cond
prefixOp t f = tok t *> (f <$> factor)

-- | Arguments parser for a CMake condition
condition :: ArgParser Cond
condition = expr

expr :: ArgParser Cond
expr = term `chainl1` orOp

term :: ArgParser Cond
term = factor `chainl1` andOp

factor :: ArgParser Cond
factor = notOp
     <|> parenthesized
     <|> unaryOp
     <|> binaryOp
     <|> constant
     <|> VariableOrString <$> item

parenthesized :: ArgParser Cond
parenthesized = Parenthesized <$> between (tok "(") (tok ")") expr

orOp :: ArgParser (Cond -> Cond -> Cond)
orOp = infixOp "OR" Or

andOp :: ArgParser (Cond -> Cond -> Cond)
andOp = infixOp "AND" And

notOp :: ArgParser Cond
notOp = prefixOp "NOT" Not

binaryOp :: ArgParser Cond
binaryOp = BinaryOp <$> item <*> binaryOpName <*> item
binaryOpName :: ArgParser BinaryOp
binaryOpName = "IN_LIST"               -$> InList
           <|> "IS_NEWER_THAN"         -$> IsNewerThan
           <|> "MATCHES"               -$> Matches
           <|> "LESS"                  -$> Less
           <|> "GREATER"               -$> Greater
           <|> "EQUAL"                 -$> Equal
           <|> "LESS_EQUAL"            -$> LessEqual
           <|> "GREATER_EQUAL"         -$> GreaterEqual
           <|> "STRLESS"               -$> StrLess
           <|> "STRGREATER"            -$> StrGreater
           <|> "STREQUAL"              -$> StrEqual
           <|> "STRLESS_EQUAL"         -$> StrGreaterEqual
           <|> "STRGREATER_EQUAL"      -$> StrGreaterEqual
           <|> "VERSION_LESS"          -$> VersionLess
           <|> "VRSION_GREATER"        -$> VersionGreater
           <|> "VERSION_EQUAL"         -$> VersionEqual
           <|> "VERSION_LESS_EQUAL"    -$> VersionLessEqual
           <|> "VERSION_GREATER_EQUAL" -$> VersionGreaterEqual

unaryOp :: ArgParser Cond
unaryOp = UnaryOp <$> unaryOpName <*> item
unaryOpName :: ArgParser UnaryOp -- FIXME should probably base on IsString to have it both ways
unaryOpName = "COMMAND"      -$> Command
          <|> "POLICY"       -$> Policy
          <|> "TARGET"       -$> Target
          <|> "TEST"         -$> Test
          <|> "DEFINED"      -$> Defined
          <|> "EXISTS"       -$> Exists
          <|> "IS_DIRECTORY" -$> IsDirectory
          <|> "IS_SYMLINK"   -$> IsSymlink
          <|> "IS_ABSOLUTE"  -$> IsAbsolute
