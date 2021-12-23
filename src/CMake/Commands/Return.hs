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
import           CMake.Interpreter.State (CmBuiltinCommand, Evasion (Return),
                                          alt, evading)
import           Control.Monad.IO.Class  (liftIO)

cmReturn :: CmBuiltinCommand
cmReturn [] _ = alt evading (const Return)
cmReturn _ callSite = fail "" <* liftIO printErr
  where
    printErr :: IO ()
    printErr = cmFormattedError FatalError
                                (Just "return")
                                ["The RETURN command does not accept any arguments"]
                                callSite
