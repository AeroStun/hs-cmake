-----------------------------------------------------------------------------
-- |
-- Copyright   :  (C) 2021 AeroStun
-- License     :  Apache-2.0
--
-- Maintainer  :  AeroStun <24841307+AeroStun@users.noreply.github.com>
-- Stability   :  experimental
-- Portability :  non-portable
--
-- Tree for CMake's condition syntax
----------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
module CMake.Cond.Defs (
  Cond(..),
  UnaryOp(..),
  BinaryOp(..),
  truthy,
  falsy
  ) where
import qualified Data.CaseInsensitive as CI
import           Data.Char            (isDigit)
import           Data.List            (isSuffixOf)

data Cond = Parenthesized Cond
          | Or Cond Cond
          | And Cond Cond
          | Not Cond
          | BinaryOp String BinaryOp String
          | UnaryOp UnaryOp String
          | Constant Bool
          | VariableOrString String
          deriving (Eq, Show)


data UnaryOp =
             -- Existence Checks
               Command
             | Policy
             | Target
             | Test
             | Defined
             -- File Operations
             | Exists
             | IsDirectory
             | IsSymlink
             | IsAbsolute
             deriving (Eq, Show)

data BinaryOp =
              -- Existence Checks
                InList
              -- File Operations
              | IsNewerThan
              -- Comparisons
              | Matches
              | Less
              | Greater
              | Equal
              | LessEqual
              | GreaterEqual
              | StrLess
              | StrGreater
              | StrEqual
              | StrLessEqual
              | StrGreaterEqual
              -- Version Comparisons
              | VersionLess
              | VersionGreater
              | VersionEqual
              | VersionLessEqual
              | VersionGreaterEqual
              deriving (Eq, Show)

-- | whether a given string is a true constant
truthy :: String -> Bool
truthy s
  | CI.mk s `elem` ["1", "ON", "YES", "TRUE", "Y"] = True
  | all (== '0') s = False
  | all isDigit s = True
truthy _ = False

-- | whether a given string is a false constant
falsy :: String -> Bool
falsy s
  | CI.mk s `elem` ["0", "OFF", "NO", "FALSE", "N", "IGNORE", "NOTFOUND", ""] = True
  | all (== '0') s = True
  |  "-NOTFOUND" `isSuffixOf` s = True -- FIXME make case-insensitive
falsy _ = False
