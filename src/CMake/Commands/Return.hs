-----------------------------------------------------------------------------
-- |
-- Copyright   :  (C) 2021 AeroStun
-- License     :  Apache-2.0
--
-- Maintainer  :  AeroStun <24841307+AeroStun@users.noreply.github.com>
-- Stability   :  experimental
-- Portability :  non-portable
--
-- CMake `return` command
----------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
module CMake.Commands.Return (cmReturn) where
import           CMake.Error             (raiseFatalError)
import           CMake.Interpreter.State (CmBuiltinCommand, Evasion (Return),
                                          alt, evading)

cmReturn :: CmBuiltinCommand
cmReturn [] _ = alt evading (const Return)
cmReturn _ cs = raiseFatalError "return" ["The RETURN command does not accept any arguments"] cs

