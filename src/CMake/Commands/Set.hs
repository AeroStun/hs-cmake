-----------------------------------------------------------------------------
-- |
-- Copyright   :  (C) 2021 AeroStun
-- License     :  Apache-2.0
--
-- Maintainer  :  AeroStun <24841307+AeroStun@users.noreply.github.com>
-- Stability   :  experimental
-- Portability :  non-portable
--
-- CMake `set` command
----------------------------------------------------------------------------
{-# LANGUAGE NamedFieldPuns #-}

module CMake.Commands.Set (set) where
import           CMake.Error             (CmErrorKind (..), cmFormattedError,
                                          raiseArgumentCountError)
import           CMake.Interpreter.State (CmBuiltinCommand, CmScope (..),
                                          CmState (..), setVariable)
import           CMake.List              (joinCmList)

set :: CmBuiltinCommand
set [] callSite _ = raiseArgumentCountError "set" callSite
set [name] _ s@CmState{currentScope} = pure $ Just s{currentScope=setVariable name "" currentScope}
set (name : values) _ s@CmState{currentScope}
  | last values /= "PARENT_SCOPE" =
      pure $ Just s{currentScope=set' name values currentScope}
set (name : _) callSite s@CmState{currentScope=CmScope{scopeParent=Nothing}} =
  Just s <$ cmFormattedError AuthorWarning (Just "set") (" Cannot set \"" ++ name ++ "\": current scope has no parent.") callSite
set (name : values) _ s@CmState{currentScope=CmScope{scopeParent=Just scope}} =
  pure $ Just s{currentScope=(currentScope s){scopeParent=Just $ set' name values scope}}

set' :: String -> [String] -> CmScope -> CmScope
set' name values = setVariable name (joinCmList values)

