-----------------------------------------------------------------------------
-- |
-- Copyright   :  (C) 2021 AeroStun
-- License     :  Apache-2.0
--
-- Maintainer  :  AeroStun <24841307+AeroStun@users.noreply.github.com>
-- Stability   :  experimental
-- Portability :  non-portable
--
-- CMake AST types
----------------------------------------------------------------------------
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}

module CMake.AST.Defs (
  SourceLocation(..),
  File,
  Statement(..),
  ConditionalChain(..),
  ConditionalBlock(..),
  ScopeBlock(..),
  CommandInvocation(..),
  Identifier(..),
  Arguments,
  Argument,
  ArgumentKind(..),
  VariableLookup(..),
  VariableReference(..),
  VariableReferenceSection(..),
  builtinLocation,
  unknownLocation
  ) where

import           Data.ByteString (ByteString)
import           Data.Int        (Int64)




-- | source location information
data SourceLocation = SourceLocation { slFile   :: !ByteString           -- ^ source file
                                     , slOffset :: {-# UNPACK #-} !Int64 -- ^ offset in file
                                     , slRow    :: {-# UNPACK #-} !Int64 -- ^ row (line) in file
                                     , slColumn :: {-# UNPACK #-} !Int64 -- ^ column in file
                                     } deriving (Eq, Ord)

instance Show SourceLocation where
  show SourceLocation{slFile, slRow} = show slFile ++ ":" ++ show slRow

-- | an abstract built-in location
builtinLocation :: SourceLocation
builtinLocation = SourceLocation "<builtin>" 0 0 0

-- | an unknown source location
unknownLocation :: SourceLocation
unknownLocation = SourceLocation "<unknown>" 0 0 0

type File = [Statement]

data Statement = InvocationStatement CommandInvocation
               | ConditionalStatement ConditionalChain
               | MacroStatement ScopeBlock
               | FunctionStatement ScopeBlock
               | ForeachStatement ScopeBlock
               | WhileStatement ConditionalBlock CommandInvocation
               deriving (Eq, Show)

-- | an ifBlock, elseifBlock* elseBlock?, endif
data ConditionalChain = ConditionalChain ConditionalBlock [ConditionalBlock] CommandInvocation deriving (Eq, Show)
-- | conditionCommand : body
data ConditionalBlock = ConditionalBlock CommandInvocation [Statement] deriving (Eq, Show)
data ScopeBlock = ScopeBlock CommandInvocation [Statement] CommandInvocation deriving (Eq, Show)

data CommandInvocation = CommandInvocation Identifier Arguments SourceLocation deriving (Eq, Show)

newtype Identifier = Identifier String deriving (Eq, Show)

type Arguments = [Argument]

type Argument = (String, ArgumentKind)
data ArgumentKind = BracketArgument
              | QuotedArgument
              | UnquotedArgument
              deriving (Eq, Show)

data VariableLookup = Scope | Cache | Env deriving (Eq, Show)
data VariableReference = VariableReference VariableLookup [VariableReferenceSection] deriving (Eq, Show)

data VariableReferenceSection = IdentifierSection String
                              | NestedReference VariableReference
                              deriving (Eq, Show)
