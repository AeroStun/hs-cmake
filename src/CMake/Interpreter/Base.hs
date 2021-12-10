-----------------------------------------------------------------------------
-- |
-- Copyright   :  (C) 2021 AeroStun
-- License     :  Apache-2.0
--
-- Maintainer  :  AeroStun <24841307+AeroStun@users.noreply.github.com>
-- Stability   :  experimental
-- Portability :  non-portable
--
-- Core CMake concepts
----------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}

module CMake.Interpreter.Base(
  truthy,
  falsy,
  immutableCommands,
  controlFlowCommands,
  controlFlowIntroducers
  ) where
import qualified Data.CaseInsensitive as CI
import           Data.Char            (isDigit)
import           Data.List            (isSuffixOf)

-- | whether a given string is a true constant
truthy :: String -> Bool
truthy s
  | CI.mk s `elem` ["1", "ON", "YES", "TRUE", "Y"] = True
  | all (== '0') s = False
  | all isDigit s = True
truthy _ = False

falsy :: String -> Bool
falsy s
  | CI.mk s `elem` ["0", "OFF", "NO", "FALSE", "N", "IGNORE", "NOTFOUND", ""] = True
  | all (== '0') s = True
  |  "-NOTFOUND" `isSuffixOf` s = True -- FIXME make case-insensitive
falsy _ = False

immutableCommands :: [String]
immutableCommands = controlFlowCommands ++ ["break", "continue", "return"]

controlFlowCommands :: [String]
controlFlowCommands = controlFlowIntroducers
                    ++ fmap ("end"++) controlFlowIntroducers
                    ++ ["elseif", "else", "break"]

controlFlowIntroducers :: [String]
controlFlowIntroducers = ["if", "foreach", "while", "function", "macro"]
