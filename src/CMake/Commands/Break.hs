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

import           CMake.Error             (raiseFatalError)
import           CMake.Interpreter.State (CmBuiltinCommand, CmScope (loopDepth),
                                          Evasion (Break), alt, currentScope,
                                          evading, sel)
import           CMakeHs.Internal.Monad  (ifM)
import           Data.Functor            ((<&>))


cmBreak :: CmBuiltinCommand
cmBreak [] cs = ifM (sel currentScope <&> loopDepth <&> (/=0))
                     (alt evading (const Break))
                     (raiseFatalError "break" ["A BREAK command was found outside of a proper FOREACH or WHILE loop scope."] cs)
cmBreak _ cs = raiseFatalError "break" ["The BREAK command does not accept any arguments."] cs
