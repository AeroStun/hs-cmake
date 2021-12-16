-----------------------------------------------------------------------------
-- |
-- Copyright   :  (C) 2021 AeroStun
-- License     :  Apache-2.0
--
-- Maintainer  :  AeroStun <24841307+AeroStun@users.noreply.github.com>
-- Stability   :  experimental
-- Portability :  non-portable
--
-- CMake `continue` command
----------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
module CMake.Commands.Continue (cmContinue) where

import           CMake.Error             (CmErrorKind (FatalError),
                                          cmFormattedError)
import           CMake.Interpreter.State (CmBuiltinCommand,
                                          CmScope (CmScope, loopDepth),
                                          CmState (CmState, currentScope, evading),
                                          Evasion (Continue))


cmContinue :: CmBuiltinCommand
cmContinue [] callSite CmState{currentScope=CmScope{loopDepth=0}} =
    Nothing <$ cmFormattedError FatalError (Just "continue") ["A CONTINUE command was found outside of a proper FOREACH or WHILE loop scope."] callSite
cmContinue [] _ s = pure $ Just s{evading=Continue}
cmContinue _ callSite _ =
    Nothing <$ cmFormattedError FatalError (Just "continue") ["The CONTINUE command does not accept any arguments."] callSite
