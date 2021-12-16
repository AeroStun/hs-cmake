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
import           CMake.Error             (CmErrorKind (FatalError),
                                          cmFormattedError)
import           CMake.Interpreter.State (CmBuiltinCommand, CmState (evading),
                                          Evasion (Return))

cmReturn :: CmBuiltinCommand
cmReturn [] _ s = pure $ Just s{evading=Return}
cmReturn _ callSite _ = Nothing <$ printErr
  where
    printErr :: IO ()
    printErr = cmFormattedError FatalError
                                (Just "return")
                                ["The RETURN command does not accept any arguments"]
                                callSite
