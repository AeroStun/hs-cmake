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
{-# LANGUAGE OverloadedStrings #-}
module CMake.Interpreter.Base(
  immutableCommands,
  controlFlowCommands,
  controlFlowIntroducers
  ) where
import           Data.ByteString       (ByteString)
import qualified Data.ByteString.Char8 as BS

immutableCommands :: [ByteString]
immutableCommands = controlFlowCommands ++ ["break", "continue", "return"]

controlFlowCommands :: [ByteString]
controlFlowCommands = controlFlowIntroducers
                    ++ fmap (BS.append "end") controlFlowIntroducers
                    ++ ["elseif", "else", "break"]

controlFlowIntroducers :: [ByteString]
controlFlowIntroducers = ["if", "foreach", "while", "function", "macro"]
