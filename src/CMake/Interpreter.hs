-----------------------------------------------------------------------------
-- |
-- Copyright   :  (C) 2021 AeroStun
-- License     :  Apache-2.0
--
-- Maintainer  :  AeroStun <24841307+AeroStun@users.noreply.github.com>
-- Stability   :  experimental
-- Portability :  non-portable
--
-- Interpreter logic
----------------------------------------------------------------------------
{-# LANGUAGE NamedFieldPuns #-}

module CMake.Interpreter (processFile, cmPrelude) where

import           CMake.AST.Defs
import           CMake.Error                 (CmError (..), CmErrorKind (..),
                                              cmFormattedError,
                                              raiseArgumentCountError)
import           CMake.Interpreter.Arguments (expandArguments)
import           CMake.Interpreter.Base      (controlFlowCommands)
import           CMake.Interpreter.State
import qualified Data.CaseInsensitive        as CI
import           Data.Functor                (($>))
import           Data.HashMap.Strict         ((!?))
import           Data.List                   (intercalate)

processFile :: File -> CmState -> IO (Maybe CmState)
processFile = processStatements

processStatements :: [Statement] -> CmState -> IO (Maybe CmState)
processStatements [] s = return $ pure s
processStatements (c : cs) s = do
  result <- processStatement c s
  case result of
    Just nextState -> processStatements cs nextState
    Nothing        -> pure Nothing

processStatement :: Statement -> CmState -> IO (Maybe CmState)
processStatement (InvocationStatement cmd) s = processInvocation cmd s
processStatement _ _                         = pure Nothing

processInvocation :: CommandInvocation -> CmState -> IO (Maybe CmState)
processInvocation (CommandInvocation (Identifier name) args callSite) s@CmState {commands, currentScope} =
  case commands !? CI.mk name of
    Just (CmFunction (ScopeBlock (CommandInvocation (Identifier cfName) _ _) stmts _)) ->
      let state = if CI.mk cfName == CI.mk "function" -- functions introduce scope, macros do not
                  then s{currentScope=emptyScope{scopeParent=Just currentScope}}
                  else s
      in processStatements stmts state
    Just (CmBuiltinCommand bc) ->
      case expandArguments args currentScope of
        Just eargs -> bc eargs callSite s
        Nothing    -> pure Nothing
    _ -> Nothing <$ cmFormattedError FatalError (Just name) ("Unknown CMake command \"" ++ name ++ "\".") callSite

cmPrelude :: CmState
cmPrelude = registerCommand "simple_message" (CmBuiltinCommand simpleMessage)
          $ registerCommand "dbg_printvar" (CmBuiltinCommand dbgPrintvar)
          $ registerCommand "set" (CmBuiltinCommand set) emptyState
  where
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
    set' name values scope = setVariable name (intercalate ";" values) scope

    dbgPrintvar :: CmBuiltinCommand
    dbgPrintvar [name] _ s@CmState{currentScope} = putStrLn (name ++ ": " ++ maybe "<unset>" (\v -> "\""++v++"\"") (readVariable name currentScope)) $> Just s
    dbgPrintvar _ caller _ = pure Nothing

    simpleMessage :: CmBuiltinCommand
    simpleMessage [] caller _ = pure Nothing
    simpleMessage msgs _ s    = putStrLn (concat msgs) $> Just s
