-----------------------------------------------------------------------------
-- |
-- Copyright   :  (C) 2021 AeroStun
-- License     :  Apache-2.0
--
-- Maintainer  :  AeroStun <24841307+AeroStun@users.noreply.github.com>
-- Stability   :  experimental
-- Portability :  non-portable
--
-- Interpreter state type tree and associated accessors/manipulators
----------------------------------------------------------------------------
{-# LANGUAGE NamedFieldPuns #-}

module CMake.Interpreter.State (
  CmState(..),
  CmScope(..),
  CmCommand(..),
  CmBuiltinCommand,
  emptyState,
  emptyScope,
  registerCommand,
  hasVariable,
  readVariable,
  setVariable
  ) where
import           CMake.AST.Defs
import           Control.Applicative  ((<|>))
import           Data.CaseInsensitive (CI)
import qualified Data.CaseInsensitive as CI
import           Data.HashMap.Strict  (HashMap, insert, (!?))
import qualified Data.HashMap.Strict  as HMap (empty)

-- | interpreter state
data CmState = CmState { currentScope :: CmScope
                       , commands     :: CommandMap
                       }

type VarMap = HashMap (CI String) String

-- | variable scope
data CmScope = CmScope { scopeIntroducer :: Maybe CommandInvocation
                       , scopeVars       :: VarMap
                       , scopeParent     :: Maybe CmScope
                       }

type CommandMap = HashMap (CI String) CmCommand

-- | invocable function
data CmCommand = CmFunction ScopeBlock
               | CmBuiltinCommand CmBuiltinCommand

type CmBuiltinCommand = [String] -> SourceLocation -> CmState -> IO (Maybe CmState)

-- | empty state
emptyState :: CmState
emptyState = CmState emptyScope HMap.empty

-- | empty scope
emptyScope :: CmScope
emptyScope = CmScope Nothing HMap.empty Nothing

-- | checks the existence of a variable in the current scope and above
hasVariable :: String -> CmScope -> Bool
hasVariable name CmScope{scopeVars, scopeParent} = elem name scopeVars || searchNext scopeParent
  where
    searchNext :: Maybe CmScope -> Bool
    searchNext Nothing      = False
    searchNext (Just scope) = hasVariable name scope

-- | read a variable's value in the current scope and above
readVariable :: String -> CmScope -> Maybe String
readVariable name CmScope{scopeVars, scopeParent} = (scopeVars !? CI.mk name) <|> searchNext scopeParent
   where
     searchNext :: Maybe CmScope -> Maybe String
     searchNext Nothing      = Nothing
     searchNext (Just scope) = readVariable name scope

-- | set a variable's value in the current scope
setVariable :: String -> String -> CmScope -> CmScope
setVariable name value s@CmScope{scopeVars} =  s{scopeVars=insert (CI.mk name) value scopeVars}

-- | registers a command by name in the state
registerCommand :: String -> CmCommand -> CmState -> CmState
registerCommand name cmd s@CmState{commands} = s{commands=insert (CI.mk name) cmd commands}
