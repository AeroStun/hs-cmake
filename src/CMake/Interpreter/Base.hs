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
-- [deprecated]
----------------------------------------------------------------------------
module CMake.Interpreter.Base(
  immutableCommands,
  controlFlowCommands,
  controlFlowIntroducers
  ) where

immutableCommands :: [String]
immutableCommands = controlFlowCommands ++ ["break", "continue", "return"]

controlFlowCommands :: [String]
controlFlowCommands = controlFlowIntroducers
                    ++ fmap ("end"++) controlFlowIntroducers
                    ++ ["elseif", "else", "break"]

controlFlowIntroducers :: [String]
controlFlowIntroducers = ["if", "foreach", "while", "function", "macro"]
