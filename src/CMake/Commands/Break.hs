-----------------------------------------------------------------------------
-- |
-- Copyright   :  (C) 2021 AeroStun
-- License     :  Apache-2.0
--
-- Maintainer  :  AeroStun <24841307+AeroStun@users.noreply.github.com>
-- Stability   :  experimental
-- Portability :  non-portable
--
-- CMake `break` command
----------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
module CMake.Commands.Break (cmBreak) where

import           CMake.Error             (CmErrorKind (FatalError),
                                          cmFormattedError)
import           CMake.Interpreter.State (CmBuiltinCommand,
                                          CmScope (CmScope, loopDepth),
                                          CmState (CmState, currentScope, evading),
                                          Evasion (Break))


cmBreak :: CmBuiltinCommand
cmBreak [] callSite CmState{currentScope=CmScope{loopDepth=0}} =
    Nothing <$ cmFormattedError FatalError (Just "break") ["A BREAK command was found outside of a proper FOREACH or WHILE loop scope."] callSite
cmBreak [] _ s = pure $ Just s{evading=Break}
cmBreak _ callSite _ =
    Nothing <$ cmFormattedError FatalError (Just "break") ["The BREAK command does not accept any arguments."] callSite
