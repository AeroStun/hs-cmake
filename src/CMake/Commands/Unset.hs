-----------------------------------------------------------------------------
-- |
-- Copyright   :  (C) 2021 AeroStun
-- License     :  Apache-2.0
--
-- Maintainer  :  AeroStun <24841307+AeroStun@users.noreply.github.com>
-- Stability   :  experimental
-- Portability :  non-portable
--
-- CMake `unset` command
----------------------------------------------------------------------------
{-# LANGUAGE NamedFieldPuns #-}

module CMake.Commands.Unset (unset) where

import           CMake.Error                 (CmErrorKind (..),
                                              cmFormattedError,
                                              raiseArgumentCountError)
import           CMake.Interpreter.Arguments (braced)
import           CMake.Interpreter.State     (CmBuiltinCommand, CmScope (..),
                                              CmState (..), scopeParent,
                                              unsetVariable)
import           Data.Functor                (($>))
import           System.Environment          (unsetEnv)


unset :: CmBuiltinCommand
unset [name] _ s@CmState{currentScope}
  | Just envVar <- braced "ENV" name = unsetEnv envVar $> Just s
  | otherwise = pure $ Just s{currentScope=unsetVariable name currentScope}
unset [_, "CACHE"] _ s = pure $ Just s -- Cache does not exist in script mode
unset [name, "PARENT_SCOPE"] callSite s@CmState{currentScope=CmScope{scopeParent}}
  | Just parentScope <- scopeParent = pure $ Just s{currentScope=(currentScope s){scopeParent=Just $ unsetVariable name parentScope}}
  | otherwise = Just s <$ cmFormattedError AuthorWarning (Just "unset") (" Cannot unset \"" ++ name ++ "\": current scope has no parent.") callSite
unset _ callSite _ = raiseArgumentCountError "unset" callSite
