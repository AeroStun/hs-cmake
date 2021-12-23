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
import           CMake.Interpreter.State (CmBuiltinCommand, CmScope (loopDepth),
                                          Evasion (Continue), alt, currentScope,
                                          evading, sel)
import           CMakeHs.Internal.Monad  (ifM)
import           Control.Monad.IO.Class  (liftIO)
import           Data.Functor            ((<&>))


cmContinue :: CmBuiltinCommand
cmContinue [] cs = ifM (sel currentScope <&> loopDepth <&> (/=0))
                     (alt evading (const Continue))
                     (liftIO (() <$ cmFormattedError FatalError (Just "continue") ["A CONTINUE command was found outside of a proper FOREACH or WHILE loop scope."] cs))
cmContinue _ cs =
     liftIO (() <$ cmFormattedError FatalError (Just "continue") ["The CONTINUE command does not accept any arguments."] cs)
